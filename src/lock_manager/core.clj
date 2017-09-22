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
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [lock-manager.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTANT! : Never call re-frame/dispatch directly, put the events in       ;;
;; re-frame channel so all events will be dispatched to re-frame in one thread ;;
;; ordered.                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def authorization-timeout 45000)
(def locking-duration? #(< 0 % 600))
(def unlocking-duration? #(< 600 % 5000))
(def ignition-button-time 2000)
(def power-off-timeout 10000)
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
(s/def :db/brake-pressed? boolean?)
(s/def :db/during-ignition? boolean?)
(s/def :db/button-pressed? boolean?)
(s/def :db/car-power-on? boolean?)
(s/def :db/reading-tag-timestamp pos-int?)
(s/def :db/authorized-since pos-int?)
(s/def :db/button-pressed-timestamp pos-int?)
(s/def ::db (s/keys :req-un [:db/door-unlock-method
                             :db/authorized-tags]
                    :opt-un [:db/reading-tag
                             :db/reading-tag-timestamp
                             :db/button-pressed-timestamp
                             :db/brake-pressed?
                             :db/authorized-since
                             :db/during-ignition?
                             :db/button-pressed?
                             :db/car-power-on?]))

(s/def :evt/initialize-db          (s/tuple #{:initialize-db}          ::db))
(s/def :evt/list-tags              (s/tuple #{:list-tags}))
(s/def :evt/rm-tag                 (s/tuple #{:rm-tag}                 :rfid.tag/id))
(s/def :evt/upsert-tag                (s/tuple #{:upsert-tag}                :db/tag))
(s/def :evt/card-on-reader         (s/tuple #{:card-on-reader}         :rfid.tag/id))
(s/def :evt/card-off-reader        (s/tuple #{:card-off-reader}        :rfid.tag/id))
(s/def :evt/set-door-unlock-method (s/tuple #{:set-door-unlock-method} :db/door-unlock-method))
(s/def :evt/button-pressed         (s/tuple #{:button-pressed}))
(s/def :evt/button-released        (s/tuple #{:button-released}))
(s/def :evt/brake-pressed          (s/tuple #{:brake-pressed}))
(s/def :evt/brake-released         (s/tuple #{:brake-released}))


(def db-file "./lock-manager-db.edn")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Re-frame events. All logic goes here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize-db-ev [{:keys [read-up-edn]} [_ initial-db]]
  {:db (or read-up-edn initial-db)})

(defn list-tags-ev [{:keys [db]} [_ answer-id]]
  {:answer [answer-id (vals (:authorized-tags db))]})

(defn upsert-tag-ev [{:keys [db]} [_ answer-id tag]]
  (let [db' (assoc-in db [:authorized-tags (:id tag)] tag)]
   {:answer [answer-id true]
    :db db'
    :write-down [db-file db']}))

(defn rm-tag-ev [{:keys [db]} [_ answer-id tag-id]]
  (let [db' (update db :authorized-tags dissoc tag-id)]
   {:answer [answer-id true]
    :db db'
    :write-down [db-file db']}))

(defn card-on-reader-ev [{:keys [db current-time-millis]} [_ tag-id]]
  {:db (cond-> (assoc db
                      :reading-tag tag-id
                      :reading-tag-timestamp current-time-millis)

               (contains? (:authorized-tags db) tag-id)
               (assoc :authorized-since current-time-millis))
   :dispatch-later [authorization-timeout [:disable-authorized]]})

(defn disable-authorized-ev [{:keys [db current-time-millis]} _]
  (when (and (:authorized-since db)
             (> (- current-time-millis (:authorized-since db))
                (- authorization-timeout 3000))) ;; just to be sure
    {:db (dissoc db :authorized-since)}))

(defn card-off-reader-ev [{:keys [db current-time-millis]} [_ tag-id]]
  (let [authorized? (contains? (:authorized-tags db) tag-id)
        duration (- current-time-millis (:reading-tag-timestamp db))
        db' (assoc db :authorized-since current-time-millis)]
    
    (when (not authorized?) (l/info "Unauthorized try for " tag-id))
    
    (cond-> {:db db}

      authorized?
      (assoc-in [:db :authorized-since] current-time-millis)
      
      (and (= (:door-unlock-method db) :both)
           authorized?
           (unlocking-duration? duration))
      (assoc :unlock-doors true)

      (and authorized?
           (locking-duration? duration))
      (assoc :lock-doors true)
      
      true
      (update :db dissoc :reading-tag))))

(defn set-door-unlock-method-ev
  "For setting a new doors unlock method."
  [{:keys [db]} [_ unlock-method]]
  {:db (assoc db :door-unlock-method unlock-method)})

(defn button-pressed-ev
  "Every time panel button is pressed."
  [{:keys [db current-time-millis]} _]
  {:db (assoc db
              :button-pressed? true
              :button-pressed-timestamp current-time-millis)
   :dispatch-later [ignition-button-time [:button-pressed-check ignition-button-time]]})

(defn button-pressed-check-ev
  [{:keys [db current-time-millis]} [_ ms]]
  (when (and (= ignition-button-time ms)
             (:car-power-on? db)
             (:button-pressed? db)
             (:brake-pressed? db))
    {:db (assoc db :during-ignition? true)
     :enable-button-ignition true
     :dispatch [:button-released]}))

(defn button-released-ev
  "Every time panel button is released."
  [{:keys [db current-time-millis]} [_]]
  (let [{:keys [car-power-on? button-pressed-timestamp]} db
        db' (-> db
                (assoc :button-pressed? false)
                (dissoc :button-pressed-timestamp))]
    (cond

      (and (not car-power-on?)
           (:authorized-since db'))
      {:db (assoc db' :car-power-on? true)
       :switch-power-on true}

      (and car-power-on?
           (:brake-pressed? db')
           (not (:during-ignition? db')))
      {:db (assoc db'
                  :car-power-on? false
                  :last-power-off-timestamp current-time-millis)
       :switch-power-off true}

      :else
      {:db db'})))

(defn brake-pressed-ev
  "Every time the car brake is pressed."
  [{:keys [db]} _]
  (let [db' (assoc db :brake-pressed? true)]
    {:db db'}))

(defn brake-released-ev
  "Every time the car brake is relased."
  [{:keys [db]} _]
  {:db (assoc db
              :brake-pressed? false
              :during-ignition? false)
   :disable-button-ignition true})

;;;;;;;;;;;;;;;
;; Component ;;
;;;;;;;;;;;;;;;

(defrecord Core [car card-reader web-server re-frame-ch])

(defn dispatch [core-cmp ev]
  (async/>!! (:re-frame-ch core-cmp) ev))

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
          (when-not (= (first ev) :tick) (l/debug "Dispatched " ev))
          (rf/dispatch ev)
          (recur)))

      (rf/reg-cofx :current-time-millis
                   (fn [cofxs] (assoc cofxs :current-time-millis (System/currentTimeMillis))))

      (rf/reg-cofx :read-up-edn
                   (fn [cofxs file-path]
                     (let [data (try
                                  (edn/read-string (slurp file-path)) 
                                  (catch Exception fnfe
                                    nil))]
                       (assoc cofxs :read-up-edn data))))
      
      ;; Register Events
      
      (rf/reg-event-fx :initialize-db [(rf/inject-cofx :read-up-edn db-file) check-spec] initialize-db-ev)
      (rf/reg-event-fx :button-pressed-check [check-spec] button-pressed-check-ev)
      (rf/reg-event-fx :disable-authorized [(rf/inject-cofx :current-time-millis) check-spec] disable-authorized-ev)
      (rf/reg-event-fx :list-tags [check-spec] list-tags-ev)
      (rf/reg-event-fx :upsert-tag [check-spec] upsert-tag-ev)
      (rf/reg-event-fx :rm-tag [check-spec] rm-tag-ev)
      (rf/reg-event-fx :card-on-reader [(rf/inject-cofx :current-time-millis) ] card-on-reader-ev)
      (rf/reg-event-fx :card-off-reader [(rf/inject-cofx :current-time-millis) ] card-off-reader-ev)
      (rf/reg-event-fx :set-door-unlock-method [ check-spec] set-door-unlock-method-ev)
      (rf/reg-event-fx :button-pressed [(rf/inject-cofx :current-time-millis)  check-spec] button-pressed-ev)
      (rf/reg-event-fx :button-released [(rf/inject-cofx :current-time-millis)  check-spec] button-released-ev)
      (rf/reg-event-fx :brake-pressed [(rf/inject-cofx :current-time-millis)  check-spec] brake-pressed-ev)
      (rf/reg-event-fx :brake-released [ check-spec] brake-released-ev)
      
      ;; Register FXs
      (rf/reg-fx :dispatch (fn [ev] (async/>!! re-frame-ch ev)))
      (rf/reg-fx :dispatch-later (fn [[ms ev]]
                                   (async/go (async/<! (async/timeout ms))
                                             (async/>! re-frame-ch ev))))
      (rf/reg-fx :enable-button-ignition (fn [_] (enable-ignition car)))
      (rf/reg-fx :disable-button-ignition (fn [_] (disable-ignition car)))
      (rf/reg-fx :lock-doors (fn [_] (lock-doors car)))
      (rf/reg-fx :unlock-doors (fn [_] (unlock-doors car)))
      (rf/reg-fx :switch-power-off (fn [_] (switch-power-off car)))
      (rf/reg-fx :switch-power-on (fn [_] (switch-power-on car)))
      (rf/reg-fx :answer (fn [[id v]]
                           (deliver (get @answers-proms id) v)))
      (rf/reg-fx :write-down (fn [[file-path data]]
                               (spit file-path data :append false)))

      ;; Events from car
      (register-brake-pressed-fn car #(async/>!! re-frame-ch [:brake-pressed]))
      (register-brake-released-fn car #(async/>!! re-frame-ch [:brake-released]))
      (register-button-pressed-fn car #(async/>!! re-frame-ch [:button-pressed]))
      (register-button-released-fn car #(async/>!! re-frame-ch [:button-released]))

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


