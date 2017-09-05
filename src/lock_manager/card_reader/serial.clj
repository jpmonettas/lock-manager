(ns lock-manager.card-reader.serial
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all]
            [serial.core :as serial]
            [serial.util :as serial-util]
            [clojure.java.io :as io]))


(defrecord Serial [read-call-back-f read-thread serial-port])

(extend-type Serial

  comp/Lifecycle

  (start [this]
    (let [serial-port (serial/open "/dev/ttyACM0" :baud-rate 9600)
          read-call-back-f (atom nil)
          serial-port-reader (atom nil)
          read-thread (Thread. (fn []
                                 (l/info "[Serial] card reader thread started")
                                 (let [reader @serial-port-reader]
                                  (try
                                    (loop [last-send (System/currentTimeMillis)]
                                      (let [tag-id (.readLine reader)
                                            now (System/currentTimeMillis)]
                                        (if (and @read-call-back-f
                                                 (> (- now last-send)
                                                    200))
                                          (do
                                            (@read-call-back-f tag-id)
                                            (recur now))
                                          (recur last-send))))
                                    (catch InterruptedException ie
                                      (l/info "[Serial] card reader thread stopped"))
                                    (catch Exception e
                                      (.printStackTrace e))))))]
      
      (serial/listen! serial-port (fn [stream]
                                    (l/info "Got first data on serial")
                                    (reset! serial-port-reader (io/reader stream))
                                    (.start read-thread)))
      
      (l/info "[Serial] card reader component started.")
      (assoc this
             :read-thread read-thread
             :serial-port serial-port
             :read-call-back-f read-call-back-f)))
  
  (stop [this]
    (.interrupt (:read-thread this))
    (serial/close! (:serial-port this))
    (l/info "[Serial] card reader component stopped.")
    (dissoc this :read-thread :call-back-f :serial-port))

  CardReaderP
  
  (register-read-fn [this f]
    (l/info "[Serial] Registeder card reader callback fn")
    (reset! (:read-call-back-f this) f)))

(defn make-serial-card-reader []
  (map->Serial {}))


