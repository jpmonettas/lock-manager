(ns lock-manager.car.genius
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [lock-manager.utils :as utils]
            [taoensso.timbre :as l]
            [lock-manager.car.protocols :refer :all]
            [clojure.core.async :as async])
  (:import com.pi4j.wiringpi.Gpio))

(def lock-door-relay-pin 2)
(def unlock-door-relay-pin 3)
(def running-led-pin 0)

(defrecord Genius [running-proc-ctrl])

(defn start-running-proc []
  (Gpio/pinMode running-led-pin Gpio/OUTPUT)
  (utils/interruptible-go-loop []
     (Gpio/digitalWrite running-led-pin Gpio/HIGH)
     (async/<! (async/timeout 1000))
     (Gpio/digitalWrite running-led-pin Gpio/LOW)
     (async/<! (async/timeout 1000))
     (recur))
  (l/info "[Genius] Running thread started"))

(extend-type Genius

  comp/Lifecycle

  (start [this]
    
    (Gpio/wiringPiSetup)
    (Gpio/pinMode lock-door-relay-pin Gpio/OUTPUT)
    (Gpio/pinMode unlock-door-relay-pin Gpio/OUTPUT)
    
    (let [running-proc-ctrl (start-running-proc)]
      
      (l/info "[Genius] component started")

      (assoc this :running-proc-ctrl running-proc-ctrl)))
  
  (stop [{:keys [running-proc-ctrl] :as this}]
    (async/>!! running-proc-ctrl :stop)
    (l/info "[Genius] component stopped")
    (assoc this :running-proc-ctrl nil))
  

  CarP

  (lock-doors [this]
    (Gpio/digitalWrite lock-door-relay-pin Gpio/HIGH)
    (async/<!! (async/timeout 1000))
    (Gpio/digitalWrite lock-door-relay-pin Gpio/LOW)
    (l/debug "Lock Wrote on pin " lock-door-relay-pin
             " for .5 sec"))
  
  (unlock-doors [this]
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/HIGH)
    (async/<!! (async/timeout 1000))
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/LOW)
    (l/debug "Unlock Wrote on pin " unlock-door-relay-pin
             " for .5 sec"))
  
  (register-break-pressed-fn [this f])
  (register-break-released-fn [this f])
  
  (switch-power-on [this])
  (switch-power-off [this])
  
  (register-button-released-fn [this f])
  (register-button-pressed-fn [this f]))

(defn make-car-genius []
  (map->Genius {}))

