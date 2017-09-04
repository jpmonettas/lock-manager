(ns lock-manager.utils
  (:require [clojure.spec.alpha :as s]))

(def byte? #(instance? java.lang.Byte %))

(defn byte-array->hex-str [byte-arr]
  (apply str (map #(format "%X" %) byte-arr)))
