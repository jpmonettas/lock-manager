(ns lock-manager.core-test
  (:require [clojure.test :refer :all]
            [lock-manager.core :as core]
            [lock-manager.car.mock :as car-mock :refer [locked?
                                                        power-on?
                                                        ignition-on?
                                                        press-brake
                                                        release-brake
                                                        press-button
                                                        release-button
                                                        make-car-mock]]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.web-server :refer [make-web-server
                                             handler]]
            [lock-manager.card-reader.protocols :refer :all]
            [lock-manager.card-reader.mock :refer [simulate-card-on
                                                   simulate-card-off
                                                   make-mock-card-reader]]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as props]
            [lock-manager.utils :as utils]
            [ring.mock.request :as mock]
            [re-frame.core :as rf]))

;; (def card-read-gen (gen/fmap (fn [[tid card-on card-off]]
;;                                   [(assoc card-on 1 tid)
;;                                    (assoc card-off 1 tid)])
;;                              (gen/tuple (gen/elements ["7564F8C2" "00000000"])
;;                                            (s/gen :evt/card-on-reader)
;;                                            (s/gen :evt/card-off-reader))))

;; (def button-press-gen (gen/tuple (s/gen :evt/button-pressed)
;;                                  (s/gen :evt/button-released)))

;; (def brake-press-gen (gen/tuple (s/gen :evt/brake-pressed)
;;                                 (s/gen :evt/brake-released)))

;; (defn build-events-gen [& types]
;;   (let [types-set (into #{} types)
;;         tuple-gens (cond-> []
;;                      (types-set :web-interaction) (conj (gen/vector (s/gen :evt/list-tags))
;;                                                         (gen/vector (s/gen :evt/rm-tag))
;;                                                         (gen/vector (s/gen :evt/add-tag)))
;;                      (types-set :brakes)          (conj (gen/fmap #(reduce into [] %) (gen/vector brake-press-gen)))
;;                      (types-set :button)          (conj (gen/fmap #(reduce into [] %) (gen/vector button-press-gen)))
;;                      (types-set :card-reads)      (conj (gen/fmap #(reduce into [] %) (gen/vector card-read-gen)))
;;                      (types-set :config)          (conj (gen/vector (s/gen :evt/set-door-unlock-method))))]
;;    (gen/fmap
;;     utils/ordered-distribute
;;     (apply gen/tuple tuple-gens))))

;; (comment
;;   (gen/generate (build-events-gen :card-reads))
;;   )

;; (rf/reg-sub :db (fn [db _] db))

;; (defspec it-should-never-turn-on-without-brake
;;   100
;;   (let [db (rf/subscribe [:db])
;;         fxs (atom {})]
;;     (rf/reg-fx :lock-doors (fn [_]  (swap! fxs update :lock-doors (fnil inc 0))))
;;     (rf/reg-fx :unlock-doors (fn [_]  (swap! fxs update :unlock-doors (fnil inc 0))))
;;     (rf/reg-fx :switch-power-off (fn [_]  (swap! fxs update :switch-power-off (fnil inc 0))))
;;     (rf/reg-fx :switch-power-on (fn [_]  (swap! fxs update :switch-power-on (fnil inc 0))))
;;     (rf/reg-fx :answer (fn [[id v]]  (swap! fxs update :answer (fnil inc 0))))

;;     (props/for-all [events-without-brake (build-events-gen :button
;;                                                            :card-reads
;;                                                            :config)
;;                     db (s/gen :lock-manager.core/db)]

;;                    (rf/dispatch-sync [:initialize-db db])
;;                    (doseq [e events-without-brake]
;;                      (rf/dispatch-sync e))
;;                    (nil? (:switch-power-on @fxs)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Integration tests ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def test-system (comp/system-map
                  :core (comp/using (core/make-core)
                                    [:car :card-reader :web-server])
                  :car (make-car-mock)
                  :card-reader (make-mock-card-reader {:ui false})
                  :web-server (make-web-server {:start-server? false})))

;; Every time start with a card locked and power-off
(defn wrap-stop-start-system [t]
  (alter-var-root #'test-system comp/start)
  (lock-doors (:car test-system))
  (switch-power-off (:car test-system))
  (core/dispatch (:core test-system)
                 [:initialize-db {:door-unlock-method :both
                                  :authorized-tags {"7564F8C2" {}}}])
  (t)
  (alter-var-root #'test-system comp/stop))

(use-fixtures :each wrap-stop-start-system)

;; Tests
(deftest test-car-doors-authorized
  (let [{:keys [card-reader car]} test-system]
    (let [tag-id "7564F8C2"
          unlocking-duration 1000 #_(gen/generate (s/gen (s/and int? core/unlocking-duration?)))
          locking-duration 500 #_(gen/generate (s/gen (s/and int? core/locking-duration?)))]
      (is (locked? car))

      (simulate-card-on card-reader tag-id)
      (Thread/sleep unlocking-duration) 
      (simulate-card-off card-reader tag-id)
      (Thread/sleep 1000)

      (is (not (locked? car)))

      (simulate-card-on card-reader tag-id)
      (Thread/sleep locking-duration) 
      (simulate-card-off card-reader tag-id)
      (Thread/sleep 1000)

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
        (Thread/sleep 1000)
        (is (power-on? car)))

      (testing "The ignition activates"
        (is (not (ignition-on? car)))
        (press-brake car)
        (press-button car)
        (Thread/sleep (+ core/ignition-button-time 1000))
        (is (ignition-on? car)))

      (testing "Ignition deactivates when brake released"
        (release-brake car)
        (Thread/sleep 1000)
        (is (not (ignition-on? car))))

      (testing "If cars stop while driving (manual transmission issue) I can start it again quickly"
        ;; drive enough so until we lost authorization
        (Thread/sleep core/authorization-timeout)
        ;; try ignition again
        (press-brake car)
        (press-button car)
        (Thread/sleep (+ core/ignition-button-time 1000))
        (is (ignition-on? car))
        (release-brake car))

      (testing "The car switches off"
        (press-brake car)
        (press-button car)
        (release-button car)
        (release-brake car)
        (Thread/sleep 1000)
        (is (not (power-on? car)))))))




