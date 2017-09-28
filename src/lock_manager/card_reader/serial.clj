(ns lock-manager.card-reader.serial
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all]
            [clojure.java.io :as io]
            [clojure.core.async :as async]
            [lock-manager.utils :as utils])
  (:import [com.fazecast.jSerialComm SerialPort]))


(defrecord Serial [call-backs reads-ch serial-port])

(extend-type Serial

  comp/Lifecycle

  (start [this]
    ;; in raspbian sudo vi /etc/udev/rules.d/10-local.rules
    ;; ACTION=="add", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", SYMLINK+="card_reader"
    (let [serial-port (doto (SerialPort/getCommPort "/dev/card_reader")
                        (.setBaudRate 9600)
                        (.setComPortTimeouts SerialPort/TIMEOUT_READ_SEMI_BLOCKING 100 0)
                        (.openPort))
          stream (.getInputStream serial-port)
          call-backs (atom {})
          reads-ch (async/chan 1 (comp (partition-by #(= % 10))
                                       (remove #(= 1 (count %)))
                                       (map #(String. (byte-array %)))))
          reader-go (atom true)]

      (let [buf (byte-array 1024)]
        (async/go-loop []
          (loop []
            (async/<! (async/timeout 100))
            (let [bytes-available (.bytesAvailable serial-port)]
              (when (> bytes-available 0)
                (let [num-read (.readBytes serial-port buf bytes-available)]
                  (doseq [b (take num-read buf)]
                    (async/>! reads-ch b))))
              (when (and (> bytes-available -1)
                       @reader-go)
                (recur))))
          (l/error "Something happened with serial port, trying to close and reopen")
          (.closePort serial-port)
          (async/<! (async/timeout 1000))
          (.openPort serial-port)
          (when @reader-go (recur))))
      
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
             :call-backs call-backs
             :reader-go reader-go)))
  
  (stop [this]
    (async/close! (:reads-ch this))
    (reset! (:reader-go this) false)
    (.closePort (:serial-port this))
    (l/info "[Serial] card reader component stopped.")
    (assoc this
           :reads-ch nil
           :call-backs nil
           :serial-port nil
           :reader-ctrl-ch nil))

  CardReaderP

  (register-card-on-reader-fn [this f]
    (swap! (:call-backs this) assoc :on-reader f))

  (register-card-off-reader-fn [this f]
    (swap! (:call-backs this) assoc :off-reader f)))

(defn make-serial-card-reader []
  (map->Serial {}))


