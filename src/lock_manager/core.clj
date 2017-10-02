(ns lock-manager.core
  (:require [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.car.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as l]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [lock-manager.utils :as utils]
            [clj-mqtt-component.core :as mqtt]))

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

(defn initialize-db-ev [{:keys [read-up-edn]} [_ _ initial-db] _]
  (or read-up-edn initial-db))

(defn list-tags-ev [_ [_ _] db]
  (with-meta db
    {:answer (vals (:authorized-tags db))}))

(defn upsert-tag-ev [_ [_ _ tag] db]
  (let [db' (assoc-in db [:authorized-tags (:id tag)] tag)]
   (with-meta db'
    {:answer true
     :write-down [db-file db']})))

(defn rm-tag-ev [_ [_ _ tag-id] db]
  (let [db' (update db :authorized-tags dissoc tag-id)]
    (with-meta db'
      {:answer true
       :write-down [db-file db']})))

(defn card-on-reader-ev [_ [_ ev-t tag-id] db]
  (let [db' (cond-> (assoc db
                           :reading-tag tag-id
                           :reading-tag-timestamp ev-t)

              (contains? (:authorized-tags db) tag-id)
              (assoc :authorized-since ev-t))]
    (with-meta db'
      {:dispatch-later [authorization-timeout [:disable-authorized]]})))

(defn disable-authorized-ev [_ [_ ev-t] db]
  (if (and (:authorized-since db)
           (> (- ev-t (:authorized-since db))
              (- authorization-timeout 1000))) ;; keep this because the sleep
                                               ;; is not exactly authorization-timeout
    (dissoc db :authorized-since)
    db))

(defn card-off-reader-ev [_ [_ ev-t tag-id] db]
  (let [authorized? (contains? (:authorized-tags db) tag-id)
        duration (- ev-t (:reading-tag-timestamp db))]
    
    (when (not authorized?) (l/info "Unauthorized try for " tag-id))
    
    (cond-> db

      authorized?
      (assoc :authorized-since ev-t)
      
      (and (= (:door-unlock-method db) :both)
           authorized?
           (unlocking-duration? duration))
      (vary-meta assoc :unlock-doors true)

      (and authorized?
           (locking-duration? duration))
      (vary-meta assoc :lock-doors true)
      
      true
      (dissoc :reading-tag))))

(defn set-door-unlock-method-ev
  "For setting a new doors unlock method."
  [_ [_ _ unlock-method] db]
  (assoc db :door-unlock-method unlock-method))

(defn button-pressed-ev
  "Every time panel button is pressed."
  [_ [_ ev-t] db]
  (let [db' (assoc db
                   :button-pressed? true
                   :button-pressed-timestamp ev-t)]
    (with-meta db'
      {:dispatch-later [ignition-button-time [:button-pressed-check ignition-button-time]]})))

(defn button-pressed-check-ev
  [_ [_ _ ms] db]
  (if (and (= ignition-button-time ms)
             (:car-power-on? db)
             (:button-pressed? db)
             (:brake-pressed? db))
    (with-meta (assoc db :during-ignition? true)
     {:enable-button-ignition true
      :dispatch [:button-released]})
    db))

(defn button-released-ev
  "Every time panel button is released."
  [_ _ db]
  (let [{:keys [car-power-on? button-pressed-timestamp]} db
        db' (-> db
                (assoc :button-pressed? false)
                (dissoc :button-pressed-timestamp))]
    (cond

      (and (not car-power-on?)
           (:authorized-since db'))
      (with-meta (assoc db' :car-power-on? true)
       {:switch-power-on true})

      (and car-power-on?
           (:brake-pressed? db')
           (not (:during-ignition? db')))
      (with-meta (assoc db'
                        :car-power-on? false)
        {:switch-power-off true})

      :else
      db')))

(defn brake-pressed-ev
  "Every time the car brake is pressed."
  [_ _ db]
  (assoc db :brake-pressed? true))

(defn brake-released-ev
  "Every time the car brake is relased."
  [_ _ db]
  (with-meta (assoc db
                    :brake-pressed? false
                    :during-ignition? false)
   {:disable-button-ignition true}))

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db))
                    (s/explain-data a-spec db)))))

(defn reduce-event [routes cofxs db [ev-k :as ev]]
  (let [[ev-cofxs reducer-fn] (get routes ev-k)
        cofxs-results (reduce (fn [r [ck & cargs]]
                                (assoc r ck (apply (get cofxs ck) cargs)))
                              {} ev-cofxs)
        db' (reducer-fn cofxs-results ev (with-meta db {}))]
    (check-and-throw ::db db')
    db'))

;;;;;;;;;;;;;;;
;; Component ;;
;;;;;;;;;;;;;;;

(defrecord Core [car card-reader mqtt state-atom opts])

(defn current-time-millis-cofx [] (System/currentTimeMillis))

(defn read-up-edn-cofx [file-path]
  (try
    (edn/read-string (slurp file-path)) 
    (catch Exception fnfe nil)))

(def ev-routes {:initialize-db [[[:read-up-edn db-file]] initialize-db-ev]
                :button-pressed-check [[] button-pressed-check-ev]
                :disable-authorized [[] disable-authorized-ev]
                :list-tags [[] list-tags-ev]
                :upsert-tag [[] upsert-tag-ev]
                :rm-tag [[] rm-tag-ev]
                :card-on-reader [[] card-on-reader-ev]
                :card-off-reader [[] card-off-reader-ev]
                :set-door-unlock-method [[] set-door-unlock-method-ev]
                :button-pressed [[] button-pressed-ev]
                :button-released [[] button-released-ev]
                :brake-pressed [[] brake-pressed-ev]
                :brake-released [[] brake-released-ev]})

(def coeffects {:read-up-edn read-up-edn-cofx})

(defn dispatch [{:keys [state-atom effects] :as core-cmp} [ev-k :as ev]]
  (l/debug "Dispatching " ev)
  (let [new-state (swap! state-atom #(reduce-event ev-routes coeffects % ev))
        ev-effects (meta new-state)]
    (when-not (empty? ev-effects) (l/debug (format "%s caused %s effects" ev (keys ev-effects))))
    #_(l/debug (format "%s changed db to %s " ev new-state))
    (doseq [[ef-k v] (dissoc ev-effects :answer)]
      (let [effect-fn (get effects ef-k)]
        (case ef-k
          :dispatch (let [[k & args] v
                          dev (into [k (System/currentTimeMillis)] args)]
                      (l/debug (format "%s re-dispatches %s" ev dev))
                      (dispatch core-cmp dev))
          :dispatch-later (let [[ms [k & args]] v]
                            (async/go (async/<! (async/timeout ms))
                                      (let [dev (into [k (System/currentTimeMillis)] args)]
                                       (l/debug (format "%s re-dispatches-later %s" ev dev))
                                       (dispatch core-cmp dev))))
          (effect-fn v))))
    (:answer ev-effects)))

(extend-type Core

  comp/Lifecycle
  
  (start [{:keys [car card-reader mqtt opts] :as this}]
    (let [state-atom (atom {})
          effects {:enable-button-ignition (fn [_] (enable-ignition car))
                   :disable-button-ignition (fn [_] (disable-ignition car))
                   :lock-doors (fn [_] (lock-doors car))
                   :unlock-doors (fn [_] (unlock-doors car))
                   :switch-power-off (fn [_] (switch-power-off car))
                   :switch-power-on (fn [_] (switch-power-on car))
                   :write-down (fn [[file-path data]]
                                 (spit file-path data :append false))}

          this (assoc this
                      :state-atom state-atom
                      :effects effects)]

      ;; Events from car
      (register-brake-pressed-fn car #(dispatch this [:brake-pressed (System/currentTimeMillis)]))
      (register-brake-released-fn car #(dispatch this [:brake-released (System/currentTimeMillis)]))
      (register-button-pressed-fn car #(dispatch this [:button-pressed (System/currentTimeMillis)]))
      (register-button-released-fn car #(dispatch this [:button-released (System/currentTimeMillis)]))

      ;; Events from card reader
      (register-card-on-reader-fn card-reader #(dispatch this [:card-on-reader (System/currentTimeMillis) %]))
      (register-card-off-reader-fn card-reader #(dispatch this [:card-off-reader (System/currentTimeMillis) %]))

      (mqtt/subscribe-and-answer mqtt (str (:car-id opts) "/method-call")
                                 (fn [[method & args :as mc]]
                                   (try
                                     (l/debug "Got from mqtt " mc)
                                     (let [answ (case method
                                                  "list-tags" {:status :ok
                                                               :val (dispatch this [:list-tags (System/currentTimeMillis)])}
                                                  "upsert-tag" {:status :ok
                                                                :val (dispatch this [:upsert-tag (System/currentTimeMillis) (first args)])}
                                                  "rm-tag" {:status :ok
                                                            :val (dispatch this [:rm-tag (System/currentTimeMillis) (first args)])}
                                                  "lock-doors" {:status :ok
                                                               :val (do (lock-doors car) true)}
                                                  "unlock-doors" {:status :ok
                                                                 :val (do (unlock-doors car) true)}
                                                  "power-on" {:status :ok
                                                              :val (do (switch-power-on car) true)}
                                                  "power-off" {:status :ok
                                                               :val (do (switch-power-off car) true)})]
                                       (l/debug "Sending back to mqtt " answ)
                                       answ)
                                     (catch Exception e
                                       (l/error e)
                                       {:status :error
                                        :error (.getMessage e)}))))
      
      (l/info "[Core] component started.")

      (dispatch this [:initialize-db {:door-unlock-method :both
                                      :authorized-tags {}}])
      this))
  
  (stop [this]
    (l/info "[Core] component stopped.")
    this))

(defn make-core [opts]
  (map->Core {:opts opts}))


