(ns lock-manager.gpio.protocols)

(defprotocol GpioP
  (config-pins [_ pins-config])
  (set-pin [_ pin-number high-or-low])
  (read-pin [_ pin-number]))
