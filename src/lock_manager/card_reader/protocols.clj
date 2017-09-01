(ns lock-manager.card-reader.protocols
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators :as tcg]
            [lock-manager.utils :refer [byte?]]))

(s/def :rfid.tag/id  (s/with-gen
                       (s/tuple byte? byte? byte? byte? byte?)
                       #(sgen/tuple tcg/byte tcg/byte tcg/byte tcg/byte tcg/byte)))

(defprotocol CardReaderP
  (register-read-fn [_ f]))
