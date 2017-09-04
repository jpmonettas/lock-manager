(ns lock-manager.card-reader.rc-522
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [lock-manager.utils :refer [byte?]]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all])
  (:import com.liangyuen.util.RaspRC522
           com.pi4j.wiringpi.Gpio))

(def running-led-pin 7)

(defrecord RC522 [read-call-back-f read-thread running-thread])

(extend-type RC522

  comp/Lifecycle

  (start [this]
    (l/info "Starting RC522 card reader component...")
    (let [rc522 (RaspRC522.)
          tag-id (byte-array 5)
          read-call-back-f (atom nil)
          read-thread (Thread. (fn []
                                 (l/info "Starting RC522 card reader thread")
                                 (try
                                   (loop [read-status nil]
                                     (when (Thread/interrupted) (throw (InterruptedException.)))
                                     (when (= read-status RaspRC522/MI_OK)
                                       (when-let [f @read-call-back-f]
                                         (f (into [] tag-id))))
                                     (Thread/sleep 200)
                                     (recur (.Select_MirareOne rc522 tag-id)))
                                   (catch InterruptedException ie
                                     (l/info "Stopping RC522 card reader thread"))
                                   (catch Exception e
                                     (.printStackTrace e)))))
          running-thread (Thread. (fn []
                                    (Gpio/pinMode running-led-pin Gpio/OUTPUT)
                                    (loop []
                                      (when-not (Thread/interrupted)
                                        (Gpio/digitalWrite running-led-pin Gpio/HIGH)
                                        (Thread/sleep 1000)
                                        (Gpio/digitalWrite running-led-pin Gpio/LOW)
                                        (Thread/sleep 1000)
                                        (recur)))))]
      (.start read-thread)
      (.start running-thread)
      (l/info "RC522 card reader component started.")
      (assoc this
             :read-thread read-thread
             :running-thread running-thread
             :read-call-back-f read-call-back-f)))
  
  (stop [this]
    (.interrupt (:read-thread this))
    (.interrupt (:running-thread this))
    (l/info "RC522 card reader component stopped.")
    (dissoc this :read-thread :call-back-f :running-thread))

  CardReaderP
  
  (register-read-fn [this f]
    (l/info "Registeder card reader callback fn")
    (reset! (:read-call-back-f this) f)))

(defn make-rc522-card-reader []
  (map->RC522 {}))


