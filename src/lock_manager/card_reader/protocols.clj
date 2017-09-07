(ns lock-manager.card-reader.protocols
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators :as tcg]
            [lock-manager.utils :refer [byte?] :as utils]))

(s/def :rfid.tag/id  (s/with-gen
                       string?
                       #(sgen/fmap
                         utils/byte-array->hex-str
                         (sgen/tuple tcg/byte tcg/byte tcg/byte tcg/byte))))

(defprotocol CardReaderP
  (register-card-on-reader-fn [_ f])
  (register-card-off-reader-fn [_ f]))
