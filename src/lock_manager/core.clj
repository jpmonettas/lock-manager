(ns lock-manager.core
  (:require [lock-manager.card-reader.protocols :refer :all]
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

(defn initialize-db-ev [{:keys [read-up-edn]} [_ initial-db] _]
  (or read-up-edn initial-db))

(defn list-tags-ev [_ [_ answer-id] db]
  (with-meta db
    {:answer (vals (:authorized-tags db))}))

(defn upsert-tag-ev [_ [_ answer-id tag] db]
  (let [db' (assoc-in db [:authorized-tags (:id tag)] tag)]
   (with-meta db'
    {:answer true
     :write-down [db-file db']})))

(defn rm-tag-ev [_ [_ answer-id tag-id] db]
  (let [db' (update db :authorized-tags dissoc tag-id)]
    (with-meta db'
      {:answer true
       :write-down [db-file db']})))

(defn card-on-reader-ev [{:keys [current-time-millis]} [_ tag-id] db]
  (let [db' (cond-> (assoc db
                           :reading-tag tag-id
                           :reading-tag-timestamp current-time-millis)

              (contains? (:authorized-tags db) tag-id)
              (assoc :authorized-since current-time-millis))]
    (with-meta db'
      {:dispatch-later [authorization-timeout [:disable-authorized]]})))

(defn disable-authorized-ev [{:keys [current-time-millis]} _ db]
  (if (and (:authorized-since db)
           (> (- current-time-millis (:authorized-since db))
              authorization-timeout))
    (dissoc db :authorized-since)
    db))

(defn card-off-reader-ev [{:keys [current-time-millis]} [_ tag-id] db]
  (let [authorized? (contains? (:authorized-tags db) tag-id)
        duration (- current-time-millis (:reading-tag-timestamp db))]
    
    (when (not authorized?) (l/info "Unauthorized try for " tag-id))
    
    (cond-> db

      authorized?
      (assoc :authorized-since current-time-millis)
      
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
  [_ [_ unlock-method] db]
  (assoc db :door-unlock-method unlock-method))

(defn button-pressed-ev
  "Every time panel button is pressed."
  [{:keys [current-time-millis]} _ db]
  (let [db' (assoc db
                   :button-pressed? true
                   :button-pressed-timestamp current-time-millis)]
    (with-meta db'
      {:dispatch-later [ignition-button-time [:button-pressed-check ignition-button-time]]})))

(defn button-pressed-check-ev
  [{:keys [current-time-millis]} [_ ms] db]
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
  [{:keys [current-time-millis]} [_] db]
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
                        :car-power-on? false
                        :last-power-off-timestamp current-time-millis)
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

;;;;;;;;;;;;;;;
;; Component ;;
;;;;;;;;;;;;;;;

(defrecord Core [car card-reader web-server state-atom])

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db))
                    (s/explain-data a-spec db)))))

(defn current-time-millis-cofx [] (System/currentTimeMillis))

(defn read-up-edn-cofx [file-path]
  (try
    (edn/read-string (slurp file-path)) 
    (catch Exception fnfe nil)))

(defn dispatch [{:keys [handlers state-atom] :as core-cmp} [ev-k :as ev]]
  (l/debug "Dispatching " ev)
  (let [[cofxs reducer-fn] (get-in handlers [:events ev-k])
        cofxs-results (reduce (fn [r [ck & cargs]]
                                (assoc r ck (apply (get-in handlers [:cofxs ck]) cargs)))
                              {} cofxs)
        new-state (swap! state-atom (fn [s]
                                      (reducer-fn cofxs-results ev (with-meta s {}))))
        effects (meta new-state)]
    (check-and-throw ::db new-state)
    (when-not (empty? effects) (l/debug (format "%s caused %s effects" ev (keys effects))))
    #_(l/debug (format "%s changed db to %s " ev new-state))
    (doseq [[ef-k v] (dissoc effects :answer)]
      (let [effect-fn (get-in handlers [:fxs ef-k])]
        (case ef-k
          :dispatch (do
                      (l/debug (format "%s re-dispatches %s" ev v))
                      (dispatch core-cmp v))
          :dispatch-later (async/go (async/<! (async/timeout (first v)))
                                    (l/debug (format "%s re-dispatches-later %s" ev (second v)))
                                    (dispatch core-cmp (second v)))
          (effect-fn v))))
    (:answer effects)))

(extend-type Core

  comp/Lifecycle
  
  (start [{:keys [car card-reader web-server] :as this}]
    (let [state-atom (atom {})
          handlers {:fxs {:enable-button-ignition (fn [_] (enable-ignition car))
                          :disable-button-ignition (fn [_] (disable-ignition car))
                          :lock-doors (fn [_] (lock-doors car))
                          :unlock-doors (fn [_] (unlock-doors car))
                          :switch-power-off (fn [_] (switch-power-off car))
                          :switch-power-on (fn [_] (switch-power-on car))
                          :write-down (fn [[file-path data]]
                                        (spit file-path data :append false))}

                    :cofxs {:current-time-millis current-time-millis-cofx
                            :read-up-edn read-up-edn-cofx}
                    :events {:initialize-db [[[:read-up-edn db-file]] initialize-db-ev]
                             :button-pressed-check [[] button-pressed-check-ev]
                             :disable-authorized [[[:current-time-millis]] disable-authorized-ev]
                             :list-tags [[] list-tags-ev]
                             :upsert-tag [[] upsert-tag-ev]
                             :rm-tag [[] rm-tag-ev]
                             :card-on-reader [[[:current-time-millis]] card-on-reader-ev]
                             :card-off-reader [[[:current-time-millis]] card-off-reader-ev]
                             :set-door-unlock-method [[] set-door-unlock-method-ev]
                             :button-pressed [[[:current-time-millis]] button-pressed-ev]
                             :button-released [[[:current-time-millis]] button-released-ev]
                             :brake-pressed [[[:current-time-millis]] brake-pressed-ev]
                             :brake-released [[] brake-released-ev]}}
          this (assoc this
                      :state-atom state-atom
                      :handlers handlers)]

      ;; Events from car
      (register-brake-pressed-fn car #(dispatch this [:brake-pressed]))
      (register-brake-released-fn car #(dispatch this [:brake-released]))
      (register-button-pressed-fn car #(dispatch this [:button-pressed]))
      (register-button-released-fn car #(dispatch this [:button-released]))

      ;; Events from card reader
      (register-card-on-reader-fn card-reader #(dispatch this [:card-on-reader %]))
      (register-card-off-reader-fn card-reader #(dispatch this [:card-off-reader %]))

      ;; Events from web server
      (register-list-tags-call-back web-server #(dispatch this [:list-tags]))
      (register-upsert-tag-call-back web-server #(dispatch this [:upsert-tag %]))
      (register-rm-tag-call-back web-server #(dispatch this [:rm-tag %]))
      
      (l/info "[Core] component started.")

      (dispatch this [:initialize-db {:door-unlock-method :both
                                      :authorized-tags {}}])
      this))
  
  (stop [this]
    (l/info "[Core] component stopped.")
    this))

(defn make-core []
  (map->Core {}))


