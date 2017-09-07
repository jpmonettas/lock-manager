(ns lock-manager.utils
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as async]))

(def byte? #(instance? java.lang.Byte %))

(defn byte-array->hex-str [byte-arr]
  (apply str (map #(format "%X" %) byte-arr)))

(defmacro interruptible-go-loop [bindings & body]
  `(let [ctrl# (async/chan)]
     (async/go-loop ~bindings
       ~@(butlast body)
       (when-not (= (async/poll! ctrl#) :stop)
         ~(last body)))
     ctrl#))


