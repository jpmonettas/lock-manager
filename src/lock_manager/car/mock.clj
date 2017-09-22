(ns lock-manager.car.mock
  (:require [lock-manager.car.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(defrecord MockCar [state call-backs])

(extend-type MockCar

  comp/Lifecycle

  (start [this]
    (l/info "[MockCar] component started")
    (assoc this
           :state (atom {:locked? true
                         :power-on? false
                         :ignition-on? false})
           :call-backs (atom {})))
  
  (stop [this]
    (l/info "[MockCar] component stopped")
    (assoc this
           :state nil
           :call-backs nil))
  
  CarP
  
  (lock-doors [{:keys [state]}]
    (swap! state assoc :locked? true)
    (l/info "Locked car door"))
  
  (unlock-doors [{:keys [state]}]
    (swap! state assoc :locked? false)
    (l/info "Unlocked car door"))

  (enable-ignition [{:keys [state]}]
    (l/debug "Enabling ignition")
    (swap! state assoc :ignition-on? true))
  
  (disable-ignition [{:keys [state]}]
    (l/debug "Disabling ignition")
    (swap! state assoc :ignition-on? false))
  
  (register-brake-pressed-fn [this f] (swap! (:call-backs this) assoc :brake-pressed f))
  (register-brake-released-fn [this f] (swap! (:call-backs this) assoc :brake-released f))
  
  (switch-power-on [{:keys [state]}]
    (l/debug "Switching car on")
    (swap! state assoc :power-on? true))
  
  (switch-power-off [{:keys [state] :as this}]
    (l/debug "Switching car off")
    (swap! state assoc :power-on? false)
    (disable-ignition this))
  
  (register-button-released-fn [this f] (swap! (:call-backs this) assoc :button-released f))
  (register-button-pressed-fn [this f] (swap! (:call-backs this) assoc :button-pressed f)))

(defn make-car-mock []
  (map->MockCar {}))

(defn locked? [mock-car-cmp] (-> mock-car-cmp :state deref :locked?))
(defn power-on? [mock-car-cmp] (-> mock-car-cmp :state deref :power-on?))
(defn ignition-on? [mock-car-cmp] (-> mock-car-cmp :state deref :ignition-on?))

(defn press-brake [mock-car-cmp]
  (when-let [bpf (-> @(:call-backs mock-car-cmp) :brake-pressed)]
    (bpf)))

(defn release-brake [mock-car-cmp]
  (when-let [brf (-> @(:call-backs mock-car-cmp) :brake-released)]
    (brf)))

(defn press-button [mock-car-cmp]
  (when-let [bpf (-> @(:call-backs mock-car-cmp) :button-pressed)]
    (bpf)))

(defn release-button [mock-car-cmp]
  (when-let [brf (-> @(:call-backs mock-car-cmp) :button-released)]
    (brf)))

