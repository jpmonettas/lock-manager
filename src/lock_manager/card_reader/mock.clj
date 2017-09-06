(ns lock-manager.card-reader.mock
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators :as tcg]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all]))

(defrecord MockCardReader [])

(extend-type MockCardReader

  comp/Lifecycle

  (start [this]
    (l/info "[MockCardReader] component started")
    (assoc this :read-call-back-fn (atom nil)))

  (stop [this]
    (l/info "[MockCardReader] component stopped")
    this)
  

  CardReaderP
  
  (register-read-fn [this f]
    (reset! (:read-call-back-fn this) f)))

(defn make-mock-card-reader []
  (map->MockCardReader {}))


(defn simulate-read
  ([card-reader-cmp] (simulate-read card-reader-cmp (sgen/generate (s/gen :rfid.tag/id))))
  ([card-reader-cmp tag-id] (simulate-read card-reader-cmp tag-id 1000))
  ([card-reader-cmp tag-id millis]
   (when-let [f @(:read-call-back-fn card-reader-cmp)]
     (let [start-time (System/currentTimeMillis)]
       (loop []
         (f tag-id)
         (Thread/sleep 200)
         (when (< (- (System/currentTimeMillis) start-time) millis)
           (recur))))
     (Thread/sleep 1000))))
