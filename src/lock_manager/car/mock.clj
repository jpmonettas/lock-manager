(ns lock-manager.car.mock
  (:require [lock-manager.car.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(defrecord MockCar [locked? power-on? call-backs])

(extend-type MockCar

  comp/Lifecycle

  (start [this]
    (l/info "[MockCar] component started")
    (assoc this
           :locked? (atom true)
           :power-on? (atom true)
           :call-backs (atom {})))
  
  (stop [this]
    (l/info "[MockCar] component stopped")
    this)
  
  CarP
  
  (lock-doors [{:keys [locked?]}]
    (reset! locked? true)
    (l/info "Locked car door"))
  
  (unlock-doors [{:keys [locked?]}]
    (reset! locked? false)
    (l/info "Unlocked car door"))
  
  (register-break-pressed-fn [this f] (swap! (:call-backs this) assoc :brake-pressed f))
  (register-break-released-fn [this f] (swap! (:call-backs this) assoc :brake-released f))
  
  (switch-power-on [{:keys [power-on?]}] (reset! power-on? true))
  (switch-power-off [{:keys [power-on?]}] (reset! power-on? false))
  
  (register-button-released-fn [this f] (swap! (:call-backs this) assoc :button-released f))
  (register-button-pressed-fn [this f] (swap! (:call-backs this) assoc :button-pressed f)))

(defn make-car-mock []
  (map->MockCar {}))

(defn locked? [mock-car-cmp] @(:locked? mock-car-cmp))

(defn power-on? [mock-car-cmp] @(:power-on? mock-car-cmp))


