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

(defn remove-from-vec [v pos]
  (-> []
      (into (subvec v 0  pos))
      (into (subvec v (inc pos)))))

(defn ordered-distribute [v]
  (->> (loop [rem (into [] (remove empty? v))
              res []]
         (if (empty? rem)
           res
           (let [sel-idx (rand-int (count rem))
                 [e & r] (nth rem sel-idx)]
             (if r
               (recur (assoc rem sel-idx r)
                      (conj res e))
               (recur (remove-from-vec rem sel-idx)
                      (conj res e))))))))


