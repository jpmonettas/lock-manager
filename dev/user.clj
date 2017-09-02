(ns user
  (:require [lock-manager.main :as main]
            [clojure.tools.namespace.repl :refer [refresh]]
            [lock-manager.card-reader.mock :refer [simulate-read]]))

(defn start-system! []
  (main/start-system {:car "mock" :card-reader "mock"})
  #_(cst/instrument))

(defn stop-system! []
  (main/stop-system))

(defn restart! []
  (stop-system!)
  (refresh :after 'user/start-system!))

(defn car-cmp [] (:car main/system))
(defn card-r-cmp [] (:card-reader main/system))

(defn s 
  ([] (simulate-read (card-r-cmp)))
  ([tag-id] (simulate-read (card-r-cmp) tag-id))
  ([tag-id millis] (simulate-read (card-r-cmp) tag-id millis)))
