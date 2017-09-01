(ns lock-manager.utils
  (:require [clojure.spec.alpha :as s]))

(def byte? #(instance? java.lang.Byte %))
