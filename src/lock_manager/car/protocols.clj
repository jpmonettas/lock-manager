(ns lock-manager.car.protocols)

(defprotocol CarP
  (lock-doors [_])
  (unlock-doors [_])
  (lock-ignition [_])
  (unlock-ignition [_]))

