(ns lock-manager.car.genius
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.car.protocols :refer :all])
  (:import com.pi4j.wiringpi.Gpio))

(def lock-door-relay-pin 28)
(def unlock-door-relay-pin 29)
(def running-led-pin 7)

(defrecord Genius [running-thread])

(extend-type Genius

  comp/Lifecycle

  (start [this]
    (let [running-thread (Thread. (fn []
                                    (Gpio/pinMode running-led-pin Gpio/OUTPUT)
                                    (loop []
                                      (when-not (Thread/interrupted)
                                        (Gpio/digitalWrite running-led-pin Gpio/HIGH)
                                        (Thread/sleep 1000)
                                        (Gpio/digitalWrite running-led-pin Gpio/LOW)
                                        (Thread/sleep 1000)
                                        (recur)))))]
      
      (Gpio/wiringPiSetup)
      (Gpio/pinMode lock-door-relay-pin Gpio/OUTPUT)
      (Gpio/pinMode unlock-door-relay-pin Gpio/OUTPUT)
      
      (.start running-thread)
      (assoc this :running-thread running-thread)))
  
  (stop [this]
    (.interrupt (:running-thread this))
    (dissoc this :running-thread))
  

  CarP

  (lock-doors [this]
    (Gpio/digitalWrite lock-door-relay-pin Gpio/HIGH)
    (Thread/sleep 1000)
    (Gpio/digitalWrite lock-door-relay-pin Gpio/LOW)
    (l/debug "Wrote on pin " lock-door-relay-pin
             " for 1 sec"))
  
  (unlock-doors [this]
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/HIGH)
    (Thread/sleep 1000)
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/LOW)
    (l/debug "Wrote on pin " unlock-door-relay-pin
             " for 1 sec"))
  
  (lock-ignition [this])
  (unlock-ignition [this])
  )

(defn make-car-genius []
  (map->Genius {}))

