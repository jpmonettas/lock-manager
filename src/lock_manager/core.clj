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
(s/def :db/reading-tag :rfid.tag/id)
(s/def :db/break-pressed boolean?)
(s/def :db/button-pressed boolean?)
(s/def :db/car-power-on? boolean?)
(s/def ::db (s/keys :req-un [:db/door-unlock-method
                             :db/authorized-tags]
                    :opt-un [:db/reading-tag
                             :db/break-pressed?
                             :db/button-pressed?
                             :db/car-power-on?]))

;; TODO! Store db middleware

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-frame events. All logic goes here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize-db-ev [_ [_ initial-db]]
  initial-db)

(defn list-tags-ev [{:keys [db]} [_ answer-id]]
  {:answer [answer-id (:authorized-tags db)]})

(defn card-on-reader-ev [db [_ tag-id]]
  (assoc db :reading-tag tag-id))

(defn card-off-reader-ev [{:keys [db]} [_ tag-id duration]]
  (l/debug tag-id " " duration " " db)
  (-> (cond
        
        (and (= (:door-unlock-method db) :both)
             (contains? (:authorized-tags db) tag-id)
             (< 600 duration 5000))
        {:unlock-doors true}

        (and (contains? (:authorized-tags db) tag-id)
             (< duration 1000))
        {:lock-doors true}
        
        (not (contains? (:authorized-tags db) tag-id))
        (do (l/info "Unauthorized try for " tag-id)
            nil))
      (assoc :db (dissoc db :reading-tag))))

(defn set-door-unlock-method-ev
  "For setting a new doors unlock method."
  [db [_ unlock-method]]
  (assoc db :door-unlock-method unlock-method))

(defn button-pressed-ev
  "Every time panel button is pressed."
  [db _]
  (assoc db :button-pressed? true))

(defn button-released-ev
  "Every time panel button is released."
  [{:keys [db]} [_ duration]]
  (let [{:keys [car-power-on?]} db
        db' (assoc-in [:db :button-pressed?] false)]
    (if (not car-power-on?)
      {:db (assoc db' :power-on? true)
       :switch-power-on true}
      {:db db'})))

(defn break-pressed-ev
  "Every time the car brake is pressed."
  [db _]
  (assoc db :break-pressed? true))

(defn break-released-ev
  "Every time the car brake is relased."
  [db _]
  (assoc db :break-pressed? false))

;;;;;;;;;;;;;;;
;; Component ;;
;;;;;;;;;;;;;;;

(defrecord Core [car card-reader web-server re-frame-ch])

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
          re-frame-ch (async/chan)]

      ;; Dispatch all re-frame events sequentially to avoid
      ;; clojure.lang.ExceptionInfo: re-frame: router state transition not found. :scheduled :finish-run
      (async/go-loop []
        (when-let [ev (async/<! re-frame-ch)]
          (rf/dispatch ev)
          (recur)))
      
      ;; Register Events
      (rf/reg-event-db :initialize-db          [debug check-spec] initialize-db-ev)
      (rf/reg-event-fx :list-tags              [debug check-spec] list-tags-ev)
      (rf/reg-event-db :card-on-reader         [debug]            card-on-reader-ev)
      (rf/reg-event-fx :card-off-reader        [debug]            card-off-reader-ev)
      (rf/reg-event-db :set-door-unlock-method [debug check-spec] set-door-unlock-method-ev)
      (rf/reg-event-db :button-pressed         [debug check-spec] button-pressed-ev)
      (rf/reg-event-fx :button-released        [debug check-spec] button-released-ev)
      (rf/reg-event-db :break-pressed          [debug check-spec] break-pressed-ev)
      (rf/reg-event-db :break-released         [debug check-spec] break-released-ev)
      
      ;; Register FXs
      (rf/reg-fx :lock-doors (fn [_] (lock-doors car)))
      (rf/reg-fx :unlock-doors (fn [_] (unlock-doors car)))
      (rf/reg-fx :switch-power-off (fn [_] (switch-power-off car)))
      (rf/reg-fx :switch-power-on (fn [_] (switch-power-on car)))
      (rf/reg-fx :answer (fn [[id v]] (deliver (get @answers-proms id) v)))

      ;; Events from car
      (register-break-pressed-fn car #(async/>!! re-frame-ch [:break-pressed]))
      (register-break-released-fn car #(async/>!! re-frame-ch [:break-released]))
      (register-button-pressed-fn car #(async/>!! re-frame-ch [:button-released %]))
      (register-button-released-fn car #(async/>!! re-frame-ch [:button-pressed]))

      ;; Events from card reader
      (register-card-on-reader-fn card-reader #(async/>!! re-frame-ch [:card-on-reader %]))
      (register-card-off-reader-fn card-reader #(async/>!! re-frame-ch [:card-off-reader %1 %2]))

      ;; Events from web server
      (register-list-tags-call-back web-server #(dispatch-for-answer answers-proms [:list-tags]))

      (async/>!! re-frame-ch [:initialize-db {:door-unlock-method :both
                                              :authorized-tags #{"7564F8C2"}}])
      
      (l/info "[Core] component started.")
      
      (assoc this
             :re-frame-ch re-frame-ch)))
  
  (stop [{:keys [re-frame-ch] :as this}]
    (async/close! re-frame-ch)
    (l/info "[Core] component stopped.")
    (assoc this
           :re-frame-ch nil)))

(defn make-core []
  (map->Core {}))


