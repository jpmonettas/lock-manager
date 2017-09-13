(ns lock-manager.car.genius
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [lock-manager.utils :as utils]
            [taoensso.timbre :as l]
            [lock-manager.car.protocols :refer :all]
            [clojure.core.async :as async])
  (:import com.pi4j.wiringpi.Gpio))

;; Output pins
(def lock-door-relay-pin 2)
(def unlock-door-relay-pin 3)
(def running-led-pin 0)
(def power-pin 1)

;; Input pins
(def brake-pin 4)
(def button-pin 5)


(defrecord Genius [running-proc-ctrl brake-proc-ctrl button-proc-ctrl])

(defn start-running-proc []
  (Gpio/pinMode running-led-pin Gpio/OUTPUT)
  (utils/interruptible-go-loop []
     (Gpio/digitalWrite running-led-pin Gpio/HIGH)
     (async/<! (async/timeout 1000))
     (Gpio/digitalWrite running-led-pin Gpio/LOW)
     (async/<! (async/timeout 1000))
     (recur))
  (l/info "[Genius] Running thread started"))

(defn start-input-proc [pin call-backs-a on-key off-key]
  (Gpio/pinMode pin Gpio/INPUT)
  (utils/interruptible-go-loop [state (Gpio/digitalRead pin)]
     (async/<! (async/timeout 100))
     (let [new-state (Gpio/digitalRead pin)]
       (if (not= state new-state)
         (do (if (= new-state 1)
               (when-let [on-fn (get @call-backs-a on-key)]
                 (on-fn)
                 (l/debug "Fired " on-key))
               (when-let [off-fn (get @call-backs-a off-key)]
                 (off-fn)
                 (l/debug "Fired " off-key)))
             (recur new-state))
         (recur state))))
  (l/info "[Genius] input thread started"))


(extend-type Genius

  comp/Lifecycle

  (start [this]
    
    (Gpio/wiringPiSetup)
    (Gpio/pinMode lock-door-relay-pin Gpio/OUTPUT)
    (Gpio/digitalWrite lock-door-relay-pin Gpio/HIGH)
    (Gpio/pinMode unlock-door-relay-pin Gpio/OUTPUT)
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/HIGH)

    (Gpio/pinMode power-pin Gpio/OUTPUT)
    (Gpio/pinMode button-pin Gpio/INPUT)
    
    (let [call-backs (atom {})
          running-proc-ctrl (start-running-proc)
          brake-proc-ctrl (start-input-proc brake-pin call-backs :brake-pressed :brake-released)
          button-proc-ctrl (start-input-proc button-pin call-backs :button-pressed :button-released)]
      (l/info "[Genius] component started")
      (assoc this
             :running-proc-ctrl running-proc-ctrl
             :brake-proc-ctrl brake-proc-ctrl
             :button-proc-ctrl button-proc-ctrl
             :call-backs call-backs)))
  
  (stop [{:keys [running-proc-ctrl brake-proc-ctrl button-proc-ctrl] :as this}]
    (async/>!! running-proc-ctrl :stop)
    (async/>!! brake-proc-ctrl :stop)
    (async/>!! button-proc-ctrl :stop)
    (l/info "[Genius] component stopped")
    (assoc this
           :running-proc-ctrl nil
           :brake-proc-ctrl nil
           :button-proc-ctrl nil
           :call-backs nil))
  

  CarP

  (lock-doors [this]
    (Gpio/digitalWrite lock-door-relay-pin Gpio/LOW)
    (async/<!! (async/timeout 600))
    (Gpio/digitalWrite lock-door-relay-pin Gpio/HIGH))
  
  (unlock-doors [this]
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/LOW)
    (async/<!! (async/timeout 600))
    (Gpio/digitalWrite unlock-door-relay-pin Gpio/HIGH))

  (switch-power-on [_]
    (Gpio/digitalWrite power-pin Gpio/HIGH))
  
  (switch-power-off [_]
    (Gpio/digitalWrite power-pin Gpio/LOW))
    
  (register-break-pressed-fn [this f] (swap! (:call-backs this) assoc :brake-pressed f))
  (register-break-released-fn [this f] (swap! (:call-backs this) assoc :brake-released f))
  
  (register-button-released-fn [this f] (swap! (:call-backs this) assoc :button-pressed f))
  (register-button-pressed-fn [this f] (swap! (:call-backs this) assoc :button-released f)))

(defn make-car-genius []
  (map->Genius {}))

