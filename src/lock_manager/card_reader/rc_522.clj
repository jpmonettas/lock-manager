(ns lock-manager.card-reader.rc-522
  (:require [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as comp]
            [lock-manager.utils :refer [byte?]]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all])
  (:import com.liangyuen.util.RaspRC522))

(defrecord RC522 [read-call-back-f read-thread])

(extend-type RC522

  comp/Lifecycle

  (start [this]
    (l/info "Starting RC522 card reader component...")
    (let [rc522 (RaspRC522.)
          tag-id (byte-array 5)
          read-thread (Thread. (fn []
                                 (l/info "Starting RC522 card reader thread")
                                 (try
                                   (loop [read-status nil]
                                     (when (Thread/interrupted) (throw (InterruptedException.)))
                                     (when (= read-status RaspRC522/MI_OK)
                                       (when-let [f @(:read-call-back-fn this)]
                                         (f (into [] tag-id))))
                                     (Thread/sleep 500)
                                     (recur (.Select_MirareOne rc522 tag-id)))
                                   (catch InterruptedException ie
                                     (l/info "Stopping RC522 card reader thread")))))]
      (.start read-thread)
      (l/info "RC522 card reader component started.")
      (assoc this
             :read-thread read-thread
             :read-call-back-f (atom nil))))
  
  (stop [this]
    (.interrupt (:read-thread this))
    (l/info "RC522 card reader component stopped.")
    (dissoc this :read-thread :call-back-f))

  CardReaderP
  
  (register-read-fn [this f]
    (reset! (:read-call-back-f this) f)))

(defn make-rc522-card-reader []
  (map->RC522 {}))


