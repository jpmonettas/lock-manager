(ns user
  (:require [lock-manager.main :as main]
            [clojure.tools.namespace.repl :refer [refresh]]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.card-reader.protocols :refer :all]))

(defn start-system! []
  (main/start-system {:car "mock" :card-reader "serial"})
  #_(cst/instrument))

(defn stop-system! []
  (main/stop-system))

(defn restart! []
  (stop-system!)
  (refresh :after 'user/start-system!))

(defn car-cmp [] (:car main/system))
(defn card-r-cmp [] (:card-reader main/system))



