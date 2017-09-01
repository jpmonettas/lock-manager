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

  (start [this] (assoc this :read-call-back-f (atom nil)))
  (stop [this] this)

  CardReaderP
  
  (register-read-fn [this f]
    (reset! (:read-call-back-f this) f)))

(defn make-mock-card-reader []
  (map->MockCardReader {}))

(defn simulate-read
  ([card-reader-cmp] (simulate-read card-reader-cmp (sgen/generate (s/gen :rfid.tag/id))))
  ([card-reader-cmp tag-id]
   (when-let [f @(:read-call-back-fn card-reader-cmp)]
     (f tag-id))))

