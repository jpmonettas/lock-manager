(ns lock-manager.card-reader.mock
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators :as tcg]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all]
            [clojure.core.async :as async]))

(defrecord MockCardReader [call-backs])

(extend-type MockCardReader

  comp/Lifecycle

  (start [this]
    (l/info "[MockCardReader] component started")
    (assoc this :call-backs (atom {})))

  (stop [this]
    (l/info "[MockCardReader] component stopped")
    this)
  

  CardReaderP
  
  (register-card-on-reader-fn [this f]
    (swap! (:call-backs this) assoc :on-reader f))

  (register-card-off-reader-fn [this f]
    (swap! (:call-backs this) assoc :off-reader f)))

(defn make-mock-card-reader []
  (map->MockCardReader {}))

(defn simulate-read
  ([card-reader-cmp] (simulate-read card-reader-cmp (sgen/generate (s/gen :rfid.tag/id))))
  ([card-reader-cmp tag-id] (simulate-read card-reader-cmp tag-id 1000))
  ([card-reader-cmp tag-id millis]
   (when-let [on-reader (get @(:call-backs card-reader-cmp) :on-reader)]
     (on-reader tag-id))
   (async/<!! (async/timeout millis))
   (when-let [off-reader (get @(:call-backs card-reader-cmp) :off-reader)]
     (off-reader tag-id))
   (async/<!! (async/timeout 1000))))
