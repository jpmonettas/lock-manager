(ns lock-manager.core-test
  (:require [clojure.test :refer :all]
            [lock-manager.core :refer :all]
            [lock-manager.car.mock :as car-mock :refer [locked?
                                                        make-car-mock]]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.web-server :refer [make-web-server]]
            [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.card-reader.mock :refer [simulate-read
                                                   make-mock-card-reader]]
            [com.stuartsierra.component :as comp]
            [lock-manager.main :as main]
            [taoensso.timbre :as l]))

(def test-system (assoc (main/create-system {})
                        :car (make-car-mock)
                        :card-reader (make-mock-card-reader)
                        :web-server (make-web-server {:start-server? false})))

;;;;;;;;;;;;;;;;;;;;;;;
;; Integration tests ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn wrap-stop-start-system [t]
  (alter-var-root #'test-system comp/start)
  (lock-doors (:car test-system))
  (switch-power-off (:car test-system))
  (t)
  (alter-var-root #'test-system comp/stop))

(use-fixtures :each wrap-stop-start-system)

(deftest test-car-should-unlock
  (let [{:keys [card-reader car]} test-system]
    (simulate-read card-reader "7564F8C2" 1500) ;; should unlock door
    (is (not (locked? car)))))
