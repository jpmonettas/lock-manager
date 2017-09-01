(ns lock-manager.car.mock
  (:require [lock-manager.car.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]))

(defrecord MockCar [])

(extend-type MockCar
  comp/Lifecycle
  (start [this] this)
  (stop [this] this)
  
  CarP
  (lock-doors [_] (l/info "Locked car door"))
  (unlock-doors [_] (l/info "Unlocked car door"))
  (lock-ignition [_] (l/info "Locked car ignition"))
  (unlock-ignition [_] (l/info "Unocked car ignition")))

(defn make-car-mock []
  (map->MockCar {}))
