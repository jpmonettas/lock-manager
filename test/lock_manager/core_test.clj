(ns lock-manager.core-test
  (:require [clojure.test :refer :all]
            [lock-manager.core :refer :all]
            [lock-manager.car.mock :as car-mock :refer [locked?
                                                        make-car-mock]]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.web-server :refer [make-web-server
                                             handler]]
            [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.card-reader.mock :refer [simulate-read
                                                   make-mock-card-reader]]
            [com.stuartsierra.component :as comp]
            [lock-manager.main :as main]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [lock-manager.utils :as utils]
            [ring.mock.request :as mock]))

(def card-read-gen (gen/fmap (fn [[tid card-on card-off]]
                                  [(assoc card-on 1 tid)
                                   (assoc card-off 1 tid)])
                             (gen/tuple (gen/elements ["7564F8C2" "00000000"])
                                           (s/gen :evt/card-on-reader)
                                           (s/gen :evt/card-off-reader))))

(def button-press-gen (gen/tuple (s/gen :evt/button-pressed)
                                 (s/gen :evt/button-released)))

(def break-press-gen (gen/tuple (s/gen :evt/break-pressed)
                                (s/gen :evt/break-released)))

(def events-gen (gen/fmap
                 utils/ordered-distribute
                 (gen/tuple (gen/vector (gen/fmap vector (s/gen :evt/list-tags)))
                            (gen/fmap #(reduce into [] %) (gen/vector card-read-gen))
                            (gen/vector (gen/fmap vector (s/gen :evt/set-door-unlock-method)))
                            (gen/fmap #(reduce into [] %) (gen/vector button-press-gen))
                            (gen/fmap #(reduce into [] %) (gen/vector break-press-gen)))))

#_(gen/generate events-gen)




;;;;;;;;;;;;;;;;;;;;;;;
;; Integration tests ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def test-system (assoc (main/create-system {})
                        :car (make-car-mock)
                        :card-reader (make-mock-card-reader)
                        :web-server (make-web-server {:start-server? false})))

;; Every time start with a card locked and power-off
(defn wrap-stop-start-system [t]
  (alter-var-root #'test-system comp/start)
  (lock-doors (:car test-system))
  (switch-power-off (:car test-system))
  (t)
  (alter-var-root #'test-system comp/stop))

(use-fixtures :each wrap-stop-start-system)

;; Tests

(deftest test-car-should-unlock-with-1500-card-swap
  (let [{:keys [card-reader car]} test-system]
    (simulate-read card-reader "7564F8C2" 1500) ;; should unlock door
    (is (not (locked? car)))))

(deftest test-car-should-not-unlock-with-300-card-swap
  (let [{:keys [card-reader car]} test-system]
    (simulate-read card-reader "7564F8C2" 300) ;; should NOT unlock door
    (is (locked? car))))


