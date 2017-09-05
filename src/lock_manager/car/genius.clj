(ns lock-manager.car.genius
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.car.protocols :refer :all])
  (:import com.pi4j.wiringpi.Gpio))

(def lock-door-relay-pin 3)
(def unlock-door-relay-pin 2)
(def running-led-pin 0)

(defrecord Genius [running-thread])

(extend-type Genius

  comp/Lifecycle

  (start [this]
    (let [running-thread (Thread. (fn []
                                    (try
                                      (l/info "[Genius] Running thread started")
                                      (Gpio/pinMode running-led-pin Gpio/OUTPUT)
                                      (loop []
                                        (when (Thread/interrupted) (throw (InterruptedException.)))
                                        (Gpio/digitalWrite running-led-pin Gpio/HIGH)
                                        (Thread/sleep 1000)
                                        (Gpio/digitalWrite running-led-pin Gpio/LOW)
                                        (Thread/sleep 1000)
                                        (recur))
                                      (catch InterruptedException ie
                                        (l/info "[Genius] Running thread stopped")))))]
      
      (Gpio/wiringPiSetup)
      (Gpio/pinMode lock-door-relay-pin Gpio/OUTPUT)
      (Gpio/pinMode unlock-door-relay-pin Gpio/OUTPUT)
      
      (.start running-thread)

      (l/info "[Genius] component started")

      (assoc this :running-thread running-thread)))
  
  (stop [this]
    (.interrupt (:running-thread this))
    (l/info "[Genius] component stopped")
    (dissoc this :running-thread))
  

  CarP

  (lock-doors [this]
    (Gpio/digitalWrite lock-door-relay-pin Gpio/HIGH)
    (Thread/sleep 500)
    (Gpio/digitalWrite lock-door-relay-pin Gpio/LOW)
    (l/debug "Wrote on pin " lock-door-relay-pin
             " for .5 sec"))
  
  (unlock-doors [this]
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/HIGH)
    (Thread/sleep 500)
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/LOW)
    (l/debug "Wrote on pin " unlock-door-relay-pin
             " for .5 sec"))
  
  (lock-ignition [this])
  (unlock-ignition [this]))

(defn make-car-genius []
  (map->Genius {}))

