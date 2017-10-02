(ns lock-manager.integration-test
  (:require [clojure.test :refer :all]
            [com.stuartsierra.component :as comp]
            [lock-manager.car.mock
             :as
             car-mock
             :refer
             [ignition-on?
              locked?
              make-car-mock
              power-on?
              press-brake
              press-button
              release-brake
              release-button]]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.card-reader.mock
             :refer
             [make-mock-card-reader simulate-card-off simulate-card-on]]
            [lock-manager.core :as core]
            [clj-mqtt-component.core :refer [make-mqtt]]))

(def test-system (comp/system-map
                  :core (comp/using (core/make-core {:car-id "testCar1"})
                                    [:car :card-reader :mqtt])
                  :car (make-car-mock)
                  :card-reader (make-mock-card-reader {:ui false})
                  :mqtt (make-mqtt {:url "tcp://127.0.0.1:1883"
                                    :car-id "testCar1"})))

;; Every time start with a card locked and power-off
(defn wrap-stop-start-system [t]
  (alter-var-root #'test-system comp/start)
  (lock-doors (:car test-system))
  (switch-power-off (:car test-system))
  (core/dispatch (:core test-system)
                 [:initialize-db 0 {:door-unlock-method :both
                                    :authorized-tags {"7564F8C2" {}}}])
  (t)
  (alter-var-root #'test-system comp/stop))

(use-fixtures :each wrap-stop-start-system)

;; Tests
(deftest test-car-doors-authorized
  (let [{:keys [card-reader car]} test-system]
    (let [tag-id "7564F8C2"
          unlocking-duration 1000 
          locking-duration 500]
      (is (locked? car))

      (simulate-card-on card-reader tag-id)
      (Thread/sleep unlocking-duration) 
      (simulate-card-off card-reader tag-id)
      (is (not (locked? car)))

      (simulate-card-on card-reader tag-id)
      (Thread/sleep locking-duration) 
      (simulate-card-off card-reader tag-id)
      (is (locked? car)))))

(deftest test-ignition-flow
  (let [{:keys [card-reader car]} test-system]
    (let [tag-id "7564F8C2"]
      ;; simulate a card swap for authorization
      (simulate-card-on card-reader tag-id)
      (simulate-card-off card-reader tag-id)

      (testing "The car powers on"
        (is (not (power-on? car)))
        (press-button car)
        (release-button car)
        (is (power-on? car)))

      (testing "The ignition activates"
        (is (not (ignition-on? car)))
        (press-brake car)
        (press-button car)
        (Thread/sleep (+ core/ignition-button-time 500))
        (is (ignition-on? car)))

      (testing "Ignition deactivates when brake released"
        (release-brake car)
        (is (not (ignition-on? car))))

      (testing "If cars stop while driving (manual transmission issue) I can start it again quickly"
        ;; drive enough so until we lost authorization
        (Thread/sleep (+ core/authorization-timeout 500))
        ;; try ignition again
        (press-brake car)
        (press-button car)
        (Thread/sleep (+ core/ignition-button-time 500))
        (is (ignition-on? car))
        (release-brake car))

      (testing "The car switches off"
        (press-brake car)
        (press-button car)
        (release-button car)
        (release-brake car)
        (is (not (power-on? car))))

      (testing "You can't power it on anymore now you don't have authorization"
        (press-button car)
        (release-button car)
        (is (not (power-on? car))))

      (testing "Swap your card again and you should be able to power the car on again"
        ;; simulate a card swap for authorization
        (simulate-card-on card-reader tag-id)
        (simulate-card-off card-reader tag-id)
        (press-button car)
        (release-button car)
        (is (power-on? car))))))
