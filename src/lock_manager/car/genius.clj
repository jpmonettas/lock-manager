(ns lock-manager.car.genius
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [lock-manager.utils :as utils]
            [taoensso.timbre :as l]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.gpio.protocols :refer :all]
            [clojure.core.async :as async]))

;; Output pins
(def running-led-pin 11)
(def lock-door-relay-pin 13)
(def unlock-door-relay-pin 15)

(def power-pin 12)
(def ignition-pin 16)

;; Input pins
(def brake-pin 18)
(def button-pin 22)


(defrecord Genius [running-proc-ctrl brake-proc-ctrl button-proc-ctrl gpio])

(defn start-running-proc [gpio]
  (l/info "[Genius] Running thread started")
  (utils/interruptible-go-loop []
    (set-pin gpio running-led-pin :high)
    (async/<! (async/timeout 1000))
    (set-pin gpio running-led-pin :low)
    (async/<! (async/timeout 1000))
    (recur)))

(defn start-input-proc [pin call-backs-a on-key off-key gpio]
  (l/info "[Genius] input thread started")
  (utils/interruptible-go-loop [state (read-pin gpio pin)]
     (async/<! (async/timeout 100))
     (let [new-state (read-pin gpio pin)]
       (if (not= state new-state)
         (do (if (= new-state :high)
               (when-let [on-fn (get @call-backs-a on-key)]
                 (on-fn)
                 (l/debug "Fired " on-key))
               (when-let [off-fn (get @call-backs-a off-key)]
                 (off-fn)
                 (l/debug "Fired " off-key)))
             (recur new-state))
         (recur state)))))


(extend-type Genius

  comp/Lifecycle

  (start [this]
    
    (let [call-backs (atom {})
          gpio (:gpio this)
          _    (config-pins gpio [[lock-door-relay-pin :out :high "lock-door"]
                                  [unlock-door-relay-pin :out :high "unlock-door"]
                                  [power-pin :out :high "power"]
                                  [ignition-pin :out :high "ignition"]
                                  [brake-pin :in nil "break"]
                                  [button-pin :in nil "button"]
                                  [running-led-pin :out nil "running"]])
          running-proc-ctrl (start-running-proc gpio)
          brake-proc-ctrl (start-input-proc brake-pin call-backs :brake-pressed :brake-released gpio)
          button-proc-ctrl (start-input-proc button-pin call-backs :button-pressed :button-released gpio)]
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

  (lock-doors [{:keys [gpio]}]

    (set-pin gpio lock-door-relay-pin :low)
    (async/<!! (async/timeout 600))
    (set-pin gpio lock-door-relay-pin :high))
  
  (unlock-doors [{:keys [gpio]}]
    (set-pin gpio unlock-door-relay-pin :low)
    (async/<!! (async/timeout 600))
    (set-pin gpio unlock-door-relay-pin :high))

  (switch-power-on [{:keys [gpio]}]
    (set-pin gpio power-pin :low))
  
  (switch-power-off [{:keys [gpio]}]
    (set-pin gpio power-pin :high))

  (enable-ignition [{:keys [gpio]}]
    (set-pin gpio ignition-pin :low))
  
  (disable-ignition [{:keys [gpio]}]
    (set-pin gpio ignition-pin :high))
  
  (register-break-pressed-fn [this f] (swap! (:call-backs this) assoc :brake-pressed f))
  (register-break-released-fn [this f] (swap! (:call-backs this) assoc :brake-released f))
  
  (register-button-pressed-fn [this f] (swap! (:call-backs this) assoc :button-pressed f))
  (register-button-released-fn [this f] (swap! (:call-backs this) assoc :button-released f)))

(defn make-car-genius []
  (map->Genius {}))

