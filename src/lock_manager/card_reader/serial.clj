(ns lock-manager.card-reader.serial
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all]
            [clojure.java.io :as io]
            [clojure.core.async :as async])
  (:import [com.fazecast.jSerialComm SerialPort]))


(defrecord Serial [call-backs reads-ch serial-port])

(extend-type Serial

  comp/Lifecycle

  (start [this]
    ;; in raspbian sudo vi /etc/udev/rules.d/10-local.rules
    ;; ACTION=="add", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", SYMLINK+="card_reader"
    (let [serial-port (doto (SerialPort/getCommPort "/dev/card_reader")
                        (.setBaudRate 9600)
                        (.openPort))
          call-backs (atom {})
          reads-ch (async/chan 1 (comp (partition-by #(= % 10))
                                       (remove #(= 1 (count %)))
                                       (map #(String. (byte-array %)))))]

      (async/go-loop []
        (let [buf (byte-array 1024)
              num-read (.readBytes serial-port buf (count buf))]
          (doseq [b (take num-read buf)]
            (async/>! reads-ch b)))
        (when (.isOpen serial-port)
          (recur)))
      
      (async/go-loop []
        (when-let [tid (async/<! reads-ch)]
          (when-let [on-reader (get @call-backs :on-reader)]
            (on-reader tid))
          (loop []
            (let [t (async/timeout 300)
                  [tag-id c] (async/alts! [reads-ch t])]
              (if (and (= c reads-ch) (= tid tag-id))
                (recur)
                (when-let [off-reader (get @call-backs :off-reader)]
                  (off-reader tid)))))
          (recur)))
      
      (l/info "[Serial] card reader component started.")
      (assoc this
             :reads-ch reads-ch
             :serial-port serial-port
             :call-backs call-backs)))
  
  (stop [this]
    (async/close! (:reads-ch this))
    (.closePort (:serial-port this))
    (l/info "[Serial] card reader component stopped.")
    (assoc this
           :reads-ch nil
           :call-backs nil
           :serial-port nil))

  CardReaderP

  (register-card-on-reader-fn [this f]
    (swap! (:call-backs this) assoc :on-reader f))

  (register-card-off-reader-fn [this f]
    (swap! (:call-backs this) assoc :off-reader f)))

(defn make-serial-card-reader []
  (map->Serial {}))


