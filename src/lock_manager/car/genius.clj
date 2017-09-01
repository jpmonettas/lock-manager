(ns lock-manager.car.genius
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.car.protocols :refer :all])
  (:import com.pi4j.wiringpi.Gpio))

(def lock-door-relay-pin 39)
(def unlock-door-relay-pin 40)

(defrecord Genius [])

(extend-type Genius

  comp/Lifecycle

  (start [this]
    (Gpio/wiringPiSetup)
    (Gpio/pinMode lock-door-relay-pin Gpio/OUTPUT)
    (Gpio/pinMode unlock-door-relay-pin Gpio/OUTPUT)
    this)
  
  (stop [this]
    this)

  CarP

  (lock-doors [this]
    (Gpio/digitalWrite lock-door-relay-pin Gpio/HIGH)
    (Thread/sleep 1000)
    (Gpio/digitalWrite lock-door-relay-pin Gpio/LOW))
  
  (unlock-doors [this]
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/HIGH)
    (Thread/sleep 1000)
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/LOW))
  
  (lock-ignition [this])
  (unlock-ignition [this])
  )

(defn make-car-genius []
  (map->Genius {}))
