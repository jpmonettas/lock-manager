(ns lock-manager.car.protocols)

(defprotocol CarP
  (lock-doors [_])
  (unlock-doors [_])

  (register-break-on-fn [_ f])
  (register-break-off-fn [_ f])
  
  (switch-power-on [_])
  (switch-power-off [_])
  
  (register-button-off-fn [_ f])
  (register-button-on-fn [_ f]))

