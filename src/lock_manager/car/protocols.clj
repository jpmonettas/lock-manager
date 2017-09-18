(ns lock-manager.car.protocols)

(defprotocol CarP
  (lock-doors [_])
  (unlock-doors [_])

  (register-brake-pressed-fn [_ f])
  (register-brake-released-fn [_ f])
  
  (switch-power-on [_])
  (switch-power-off [_])
  (enable-ignition [_])
  (disable-ignition [_])
  
  (register-button-released-fn [_ f])
  (register-button-pressed-fn [_ f]))

