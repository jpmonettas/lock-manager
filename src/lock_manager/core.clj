(ns lock-manager.core
  (:require [re-frame.core :as rf :refer [debug]]
            [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.web-server :refer [register-upsert-tag-call-back
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

(s/def :db.tag.interval/from-hour (s/int-in 0 24))
(s/def :db.tag.interval/to-hour (s/int-in 0 24))
(s/def :db.tag/owner-name (s/and string? not-empty))
(s/def :db.tag/intervals (s/coll-of (s/keys :req-un [:db.tag.interval/from-hour
                                                     :db.tag.interval/to-hour])
                                    :kind vector?
                                    :gen-max 3))
(s/def :db/tag (s/keys :req-un [:rfid.tag/id
                                :db.tag/owner-name
                                :db.tag/intervals]))
(s/def :db/door-unlock-method #{:both :only-key})
(s/def :db/authorized-tags (s/map-of :rfid.tag/id :db/tag))
(s/def :db/reading-tag :rfid.tag/id)
(s/def :db/break-pressed? boolean?)
(s/def :db/button-pressed? boolean?)
(s/def :db/car-power-on? boolean?)
(s/def :db/reading-tag-timestamp pos-int?)
(s/def :db/button-pressed-timestamp pos-int?)
(s/def ::db (s/keys :req-un [:db/door-unlock-method
                             :db/authorized-tags]
                    :opt-un [
                             :db/reading-tag
                             :db/reading-tag-timestamp
                             :db/button-pressed-timestamp
                             :db/break-pressed?
                             :db/button-pressed?
                             :db/car-power-on?]))

(s/def :evt/initialize-db          (s/tuple #{:initialize-db}          ::db))
(s/def :evt/list-tags              (s/tuple #{:list-tags}))
(s/def :evt/rm-tag                 (s/tuple #{:rm-tag}                 :rfid.tag/id))
(s/def :evt/add-tag                (s/tuple #{:add-tag}                :db/tag))
(s/def :evt/card-on-reader         (s/tuple #{:card-on-reader}         :rfid.tag/id))
(s/def :evt/card-off-reader        (s/tuple #{:card-off-reader}        :rfid.tag/id))
(s/def :evt/set-door-unlock-method (s/tuple #{:set-door-unlock-method} :db/door-unlock-method))
(s/def :evt/button-pressed         (s/tuple #{:button-pressed}))
(s/def :evt/button-released        (s/tuple #{:button-released}))
(s/def :evt/break-pressed          (s/tuple #{:break-pressed}))
(s/def :evt/break-released         (s/tuple #{:break-released}))

;; TODO! Store db middleware

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-frame events. All logic goes here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize-db-ev [_ [_ initial-db]]
  {:db initial-db})

(defn list-tags-ev [{:keys [db]} [_ answer-id]]
  {:answer [answer-id (vals (:authorized-tags db))]})

(defn upsert-tag-ev [{:keys [db]} [_ answer-id tag]]
  {:answer [answer-id true]
   :db (assoc-in db [:authorized-tags (:id tag)] tag)})

(defn rm-tag-ev [{:keys [db]} [_ answer-id tag-id]]
  {:answer [answer-id true]
   :db (update db :authorized-tags dissoc tag-id)})

(defn card-on-reader-ev [{:keys [db current-time-millis]} [_ tag-id]]
  {:db (assoc db
              :reading-tag tag-id
              :reading-tag-timestamp current-time-millis)})

(defn current-reading-authorized? [db]
  (and (:reading-tag db)
       (contains? (:authorized-tags db)
                  (:reading-tag db))))

(defn card-off-reader-ev [{:keys [db current-time-millis]} [_ tag-id]]
  (let [authorized? (current-reading-authorized? db)
        duration (- current-time-millis (:reading-tag-timestamp db))]
   (-> (cond
        
         (and (= (:door-unlock-method db) :both)
              authorized?
              (< 600 duration 5000))
         {:unlock-doors true}

         (and authorized?
              (< duration 1000))
         {:lock-doors true}
        
         (not authorized?)
         (do (l/info "Unauthorized try for " tag-id)
             nil))
       (assoc :db (dissoc db :reading-tag)))))

(defn set-door-unlock-method-ev
  "For setting a new doors unlock method."
  [{:keys [db]} [_ unlock-method]]
  {:db (assoc db :door-unlock-method unlock-method)})

(defn button-pressed-ev
  "Every time panel button is pressed."
  [{:keys [db current-time-millis]} _]
  {:db (assoc db
              :button-pressed? true
              :button-pressed-timestamp current-time-millis)})

(defn button-released-ev
  "Every time panel button is released."
  [{:keys [db current-time-millis]} [_]]
  (let [{:keys [car-power-on? button-pressed-timestamp]} db
        duration (- current-time-millis button-pressed-timestamp)
        db' (assoc db :button-pressed? false)]
    (cond

      (and (not car-power-on?)
           (current-reading-authorized? db))
      {:db (assoc db' :power-on? true)
       :switch-power-on true}

      (and car-power-on?
           (:break-pressed? db))
      {:db (assoc db' :power-on? false)
       :switch-power-off true}

      :else
      {:db db'})))

(defn break-pressed-ev
  "Every time the car brake is pressed."
  [{:keys [db]} _]
  (when (current-reading-authorized? db)
    (let [db' (assoc db :break-pressed? true)]
      (if (:power-on? db')
        {:enable-button-ignition true
         :db db'}
        
        {:db db'}))))

(defn break-released-ev
  "Every time the car brake is relased."
  [{:keys [db]} _]
  (when (current-reading-authorized? db)
    {:db (assoc db :break-pressed? false)
     :disable-button-ignition true}))

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

      (rf/reg-cofx :current-time-millis
                   (fn [cofxs] (assoc cofxs :current-time-millis (System/currentTimeMillis))))
      
      ;; Register Events
      
      (rf/reg-event-fx :initialize-db [ check-spec] initialize-db-ev)
      (rf/reg-event-fx :list-tags [ check-spec] list-tags-ev)
      (rf/reg-event-fx :upsert-tag [ check-spec] upsert-tag-ev)
      (rf/reg-event-fx :rm-tag [ check-spec] rm-tag-ev)
      (rf/reg-event-fx :card-on-reader [(rf/inject-cofx :current-time-millis) ] card-on-reader-ev)
      (rf/reg-event-fx :card-off-reader [(rf/inject-cofx :current-time-millis) ] card-off-reader-ev)
      (rf/reg-event-fx :set-door-unlock-method [ check-spec] set-door-unlock-method-ev)
      (rf/reg-event-fx :button-pressed [(rf/inject-cofx :current-time-millis)  check-spec] button-pressed-ev)
      (rf/reg-event-fx :button-released [(rf/inject-cofx :current-time-millis)  check-spec] button-released-ev)
      (rf/reg-event-fx :break-pressed [(rf/inject-cofx :current-time-millis)  check-spec] break-pressed-ev)
      (rf/reg-event-fx :break-released [ check-spec] break-released-ev)
      
      ;; Register FXs
      (rf/reg-fx :lock-doors (fn [_] (lock-doors car)))
      (rf/reg-fx :unlock-doors (fn [_] (unlock-doors car)))
      (rf/reg-fx :switch-power-off (fn [_] (switch-power-off car)))
      (rf/reg-fx :switch-power-on (fn [_] (switch-power-on car)))
      (rf/reg-fx :answer (fn [[id v]]
                           (deliver (get @answers-proms id) v)))

      ;; Events from car
      (register-break-pressed-fn car #(async/>!! re-frame-ch [:break-pressed]))
      (register-break-released-fn car #(async/>!! re-frame-ch [:break-released]))
      (register-button-pressed-fn car #(async/>!! re-frame-ch [:button-released]))
      (register-button-released-fn car #(async/>!! re-frame-ch [:button-pressed]))

      ;; Events from card reader
      (register-card-on-reader-fn card-reader #(async/>!! re-frame-ch [:card-on-reader %]))
      (register-card-off-reader-fn card-reader #(async/>!! re-frame-ch [:card-off-reader %]))

      ;; Events from web server
      (register-list-tags-call-back web-server #(dispatch-for-answer answers-proms re-frame-ch [:list-tags]))
      (register-upsert-tag-call-back web-server #(dispatch-for-answer answers-proms re-frame-ch [:upsert-tag %]))
      (register-rm-tag-call-back web-server #(dispatch-for-answer answers-proms re-frame-ch [:rm-tag %]))
      
      (async/>!! re-frame-ch [:initialize-db {:door-unlock-method :both
                                              :authorized-tags {}}])
      
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


