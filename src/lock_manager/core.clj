(ns lock-manager.core
  (:require [re-frame.core :as rf :refer [debug]]
            [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.car.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as l]))


(defrecord Core [car card-reader])

(extend-type Core

  comp/Lifecycle
  (start [{:keys [car card-reader] :as this}]
    
    (rf/reg-fx :lock-doors #(lock-doors car))
    (rf/reg-fx :unlock-doors (fn [_] (unlock-doors car)))
    (rf/reg-fx :lock-ignition #(lock-ignition car))
    (rf/reg-fx :unlock-ignition #(unlock-ignition car))
    
    (register-read-fn card-reader #(rf/dispatch [:card-read %]))

    (rf/dispatch-sync [:initialize-db {::door-unlock-method :both}])

    (rf/reg-cofx :authorized-tags
                 (fn [cofxs] (assoc cofxs :authorized-tags #{[0 0 0 0 0]})))
    this)
  
  (stop [this] this))

(defn make-core []
  (map->Core {}))

(s/def ::door-unlock-method #{:both :only-key})
(s/def ::db (s/keys :req [::door-unlock-method]))

(rf/reg-event-db
 :initialize-db
 [debug]
 (fn [_ [_ initial-db]]
   initial-db))

(rf/reg-event-fx
 :card-read
 [debug (rf/inject-cofx :authorized-tags)]
 (fn [{:keys [db authorized-tags]} [_ tag-id]]
   (if (and (= (::door-unlock-method db) :both)
            (contains? authorized-tags tag-id))
     {:unlock-doors true}

     (do (l/info "Unauthorized try for " tag-id)
         nil))))

(rf/reg-event-db
 :set-door-unlock-method
 [debug]
 (fn [db [_ unlock-method]]
   (assoc db ::door-unlock-method unlock-method)))

(comment

  (rf/dispatch [:card-read [1 1 1 1 1]])
  (rf/dispatch [:card-read [0 0 0 0 0]])
  )
