(ns lock-manager.core
  (:require [re-frame.core :as rf :refer [debug]]
            [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.web-server :refer [register-add-tag-call-back
                                             register-list-tags-call-back
                                             register-rm-tag-call-back]]
            [com.stuartsierra.component :as comp]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as l]
            [clojure.string :as str]
            [clojure.core.async :as async]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTANT! : Never call re-frame/dispatch directly, put the events in       ;;
;; re-frame channel so all events will be dispatched to re-frame in one thread ;;
;; ordered.                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; Db Spec ;;
;;;;;;;;;;;;;

(s/def :db/door-unlock-method #{:both :only-key})
(s/def :db/authorized-tags (s/coll-of :rfid.tag/id :kind set?))
(s/def :db.card-reader/first-read int?)
(s/def :db.card-reader/last-read int?)
(s/def :db/card-reader (s/keys :req-un [:rfid.tag/id
                                        :db.card-reader/first-read]
                               :opt-un [:db/card-reader/last-read]))
(s/def ::db (s/keys :req-un [:db/door-unlock-method
                             :db/authorized-tags]
                    :opt-un [:db/card-reader]))

;; TODO! Store db middleware

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-frame events. All logic goes here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize-db-ev [_ [_ initial-db]]
  initial-db)

(defn list-tags-ev [{:keys [db]} [_ answer-id]]
  {:answer [answer-id (:authorized-tags db)]})

(defn card-read-ev [db [_ tag-id time-millis]]
  (let [db' (assoc-in db [:card-reader :tag-id] tag-id)]
    (if (-> db' :card-reader :first-read)
      (assoc-in db' [:card-reader :last-read] time-millis)
      (assoc-in db' [:card-reader :first-read] time-millis))))

(defn tick-ev [{:keys [db]} [_ time-millis]]
  (when-let [first-read (-> db :card-reader :first-read)]
    (let [last-read (or (-> db :card-reader :last-read) first-read)
          reading-time  (- last-read first-read)]
      (when (> (- time-millis last-read) 500)
        {:db (dissoc db :card-reader)
         :dispatch [:card-full-read (-> db :card-reader :tag-id) reading-time]}))))

(defn card-full-read-ev [{:keys [db]} [_ tag-id duration]]
  (cond

    (and (= (:door-unlock-method db) :both)
         (contains? (:authorized-tags db) tag-id)
         (< 600 duration 5000))
    {:unlock-doors true}

    (and (contains? (:authorized-tags db) tag-id)
         (< duration 1000))
    {:lock-doors true}
    
    (not (contains? (:authorized-tags db) tag-id))
    (do (l/info "Unauthorized try for " tag-id)
        nil)))

(defn set-door-unlock-method-ev [db [_ unlock-method]]
  (assoc db :door-unlock-method unlock-method))

;;;;;;;;;;;;;;;
;; Component ;;
;;;;;;;;;;;;;;;

(defrecord Core [car card-reader web-server ticks-thread re-frame-ch])

(defn dispatch-for-answer [answers-proms-a re-frame-ch [ev-id & ev-args]]
  (let [p (promise)
        pid (rand-int 1000)]
    (swap! answers-proms-a assoc pid p)
    (async/>!! re-frame-ch (into [ev-id pid] ev-args))
    @p))

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db _]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db))
                    (s/explain-data a-spec db)))))

(def check-spec (rf/after (partial check-and-throw ::db)))

(extend-type Core

  comp/Lifecycle
  
  (start [{:keys [car card-reader web-server] :as this}]
    (let [answers-proms (atom {})
          re-frame-ch (async/chan)
          ticks-thread (Thread.
                        (fn []
                          (try
                            (l/info "[Core] Ticks thread started")
                            (loop []
                              (when (Thread/interrupted) (throw (InterruptedException.)))
                              (async/>!! re-frame-ch [:tick (System/currentTimeMillis)])
                              (Thread/sleep 100)
                              (recur))
                           (catch InterruptedException ie
                             (l/info "[Core] Ticks thread stopped")))))]

      ;; Dispatch all re-frame events sequentially to avoid
      ;; clojure.lang.ExceptionInfo: re-frame: router state transition not found. :scheduled :finish-run
      (async/go-loop []
        (when-let [ev (async/<! re-frame-ch)]
          (rf/dispatch ev)
          (recur)))
      
      ;; Register Events
      (rf/reg-event-db :initialize-db          [debug check-spec] initialize-db-ev)
      (rf/reg-event-fx :list-tags              [debug check-spec] list-tags-ev)
      (rf/reg-event-db :card-read              [debug]            card-read-ev)
      (rf/reg-event-fx :tick                   []                 tick-ev)
      (rf/reg-event-fx :card-full-read         [debug check-spec] card-full-read-ev)
      (rf/reg-event-db :set-door-unlock-method [debug check-spec] set-door-unlock-method-ev)
      
      ;; Register FXs
      (rf/reg-fx :lock-doors (fn [_] (lock-doors car)))
      (rf/reg-fx :unlock-doors (fn [_] (unlock-doors car)))
      (rf/reg-fx :answer (fn [[id v]] (deliver (get @answers-proms id) v)))
     
      ;; Events from card reader
      (register-read-fn card-reader #(async/>!! re-frame-ch [:card-read % (System/currentTimeMillis)]))

      ;; Events from web server
      (register-list-tags-call-back web-server #(dispatch-for-answer answers-proms [:list-tags]))

      (async/>!! re-frame-ch [:initialize-db {:dooor-unlock-method :both
                                              :authorized-tags #{"7564F8C2"}}])
      (.start ticks-thread)

      (l/info "[Core] component started.")
      (assoc this
             :ticks-thread ticks-thread
             :re-frame-ch re-frame-ch)))
  
  (stop [{:keys [ticks-thread re-frame-ch] :as this}]
    (async/close! re-frame-ch)
    (.interrupt ticks-thread)
    (l/info "[Core] component stopped.")
    (assoc this
           :ticks-thread nil
           :re-frame-ch nil)))

(defn make-core []
  (map->Core {}))


