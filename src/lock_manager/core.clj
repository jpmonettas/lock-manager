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
            [clojure.string :as str]))


(defrecord Core [car card-reader web-server ticks-thread])

(defn dispatch-for-answer [answers-proms-a [ev-id & ev-args]]
  (let [p (promise)
        pid (rand-int 1000)]
    (swap! answers-proms-a assoc pid p)
    (rf/dispatch (into [ev-id pid] ev-args))
    @p))

(extend-type Core

  comp/Lifecycle
  
  (start [{:keys [car card-reader web-server ticks-thread] :as this}]
    (let [answers-proms (atom {})
          ticks-thread (Thread.
                        (fn []
                          (rf/dispatch [:tick (System/currentTimeMillis)])
                          (Thread/sleep 100)
                          (when-not (Thread/interrupted) (recur))))]
      
      ;;  FXs
      (rf/reg-fx :lock-doors #(lock-doors car))
      (rf/reg-fx :unlock-doors (fn [_] (unlock-doors car)))
      (rf/reg-fx :lock-ignition #(lock-ignition car))
      (rf/reg-fx :unlock-ignition #(unlock-ignition car))
      (rf/reg-fx :answer (fn [[id v]] (deliver (get @answers-proms id) v)))
     #_(rf/reg-cofx :current-time-millis (fn [cofx] (assoc cofx :current-time-millis (System/currentTimeMillis))))
      
      ;; Events from card reader
      (register-read-fn card-reader #(rf/dispatch [:card-read % (System/currentTimeMillis)]))

      ;; Events from web server
      (register-list-tags-call-back web-server #(dispatch-for-answer answers-proms [:list-tags]))

      (rf/dispatch [:initialize-db {::door-unlock-method :both
                                    ::authorized-tags #{[0 0 0 0 0]}}])
      (.start ticks-thread)
      (assoc this :ticks-thread ticks-thread)))
  
  (stop [{:keys [ticks-thread] :as this}]
    (.interrupt ticks-thread)
    (dissoc this :ticks-thread)))

(defn make-core []
  (map->Core {}))

;;;;;;;;;;;;;
;; Db Spec ;;
;;;;;;;;;;;;;

(s/def ::door-unlock-method #{:both :only-key})
(s/def ::authorized-tags (s/coll-of :rfid.tag/id :kind set?))
(s/def ::db (s/keys :req [::door-unlock-method
                          ::authorized-tags]))

;; TODO! Store db middleware

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-frame. All logic goes here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rf/reg-event-db
 :initialize-db
 [debug]
 (fn [_ [_ initial-db]]
   initial-db))

(rf/reg-event-fx
 :list-tags
 [debug]
 (fn [{:keys [db]} [_ answer-id]]
   {:answer [answer-id (::authorized-tags db)]}))

(rf/reg-event-db
 :card-read
 (fn [db [_ tag-id time-millis]]
   (let [db' (assoc-in db [::card-reader :tag-id] tag-id)]
     (if (-> db' ::card-reader :first-read)
       (assoc-in db' [::card-reader :last-read] time-millis)
       (assoc-in db' [::card-reader :first-read] time-millis)))))

(rf/reg-event-fx
 :tick
 (fn [{:keys [db]} [_ time-millis]]
   (when-let [first-read (-> db ::card-reader :first-read)]
     (let [last-read (or (-> db ::card-reader :last-read) first-read)
           reading-time  (- last-read first-read)]
       (when (> (- time-millis last-read) 500)
         {:db (dissoc db ::card-reader)
          :dispatch [:card-full-read (-> db ::card-reader :tag-id) reading-time]})))))

(rf/reg-event-fx
 :card-full-read
 [debug]
 (fn [{:keys [db]} [_ tag-id duration-millis]]
   (if (and (= (::door-unlock-method db) :both)
            (contains? (::authorized-tags db) tag-id))
     {:unlock-doors true}

     (do (l/info "Unauthorized try for " tag-id)
         nil))))

(rf/reg-event-db
 :set-door-unlock-method
 [debug]
 (fn [db [_ unlock-method]]
   (assoc db ::door-unlock-method unlock-method)))

(comment

  (rf/dispatch [:card-read [1 1 1 1 1] (System/currentTimeMillis)])
  (rf/dispatch [:card-read [0 0 0 0 0] (System/currentTimeMillis)])
  )
