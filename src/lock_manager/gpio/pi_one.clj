(ns lock-manager.gpio.pi-one
  (:require [lock-manager.gpio.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l])
  (:import com.pi4j.wiringpi.Gpio))

(defrecord PiOneGpio [])

(extend-type PiOneGpio
  comp/Lifecycle
  (start [this]
    (Gpio/wiringPiSetupPhys)
    this)
  (stop [this] this)

  GpioP

  (config-pins [this pins-config]
    (doseq [[n mode initial label] pins-config]
      (Gpio/pinMode n ({:out Gpio/OUTPUT
                        :in Gpio/INPUT} mode))
      (when initial
       (if (= mode :out)
         (set-pin this n initial)
         (Gpio/pullUpDnControl n ({:high Gpio/PUD_UP
                                   :low Gpio/PUD_DOWN} initial))))))
  
  (set-pin [this pin-number high-or-low]
    (Gpio/digitalWrite pin-number ({:high Gpio/HIGH
                                    :low Gpio/LOW}
                                   high-or-low)))
  (read-pin [this pin-number]
    (let [v ({0 :low
              1 :high} (Gpio/digitalRead pin-number))]
      v)))

(defn make-pi-one-gpio []
  (map->PiOneGpio {}))
