(ns lock-manager.main
  (:require [clojure.tools.nrepl.server :as nrepl]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [com.stuartsierra.component :as comp]
            [lock-manager.card-reader.mock :refer [make-mock-card-reader]]
            [lock-manager.card-reader.serial :refer [make-serial-card-reader]]
            [lock-manager.car.mock :refer [make-car-mock]]
            [lock-manager.car.genius :refer [make-car-genius]]
            [lock-manager.core :refer [make-core]]
            [lock-manager.web-server :refer [make-web-server]]
            [lock-manager.gpio.pi-one :refer [make-pi-one-gpio]]
            [lock-manager.gpio.mock :refer [make-mock-gpio]]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as str]
            [taoensso.timbre.appenders.core :refer [println-appender]]
            [taoensso.timbre.appenders.3rd-party.rotor :refer [rotor-appender]])
  (:gen-class))

(def system nil)

(defn create-system [opts]
  (comp/system-map
   :card-reader (case (:card-reader opts)
                  "serial" (make-serial-card-reader)
                  (make-mock-card-reader))
   :car (comp/using (make-car-genius)
                    [:gpio])
   :web-server (make-web-server {:start-server? true})
   :core (comp/using (make-core)
                     [:car :card-reader :web-server])
   :gpio (case (:gpio opts)
           "pione" (make-pi-one-gpio)
           (make-mock-gpio))))

(s/def ::card-reader-opt (s/cat :pref #{"--card-reader"}
                                :val #{"serial" "mock"}))
(s/def ::gpio-opt (s/cat :pref #{"--gpio"}
                         :val #{"pione" "mock"}))
(s/def ::args (s/* (s/alt :card-reader ::card-reader-opt
                          :gpio ::gpio-opt)))

(defn start-system [opts]
  (alter-var-root #'system (fn [s] (comp/start (create-system opts)))))

(defn stop-system []
  (alter-var-root #'system (fn [s]
                             (when s (comp/stop s)))))

(defn -main
  [& args]

  (l/merge-config! {:log-level :debug 
                    :appenders {:rotor (rotor-appender {:path "lock-manager.log"})}})


  (let [conformed-opts (s/conform ::args args)]
    (if (not= conformed-opts ::s/invalid)
      (let [opts (into {} conformed-opts)
            opts' {:card-reader (-> opts :card-reader :val)
                   :gpio (-> opts :gpio :val)}]

        (nrepl/start-server :handler cider-nrepl-handler
                            :port 7778
                            :bind "0.0.0.0")
        (l/info "Nrepl server started.")


        (start-system opts')
        
        (l/info "System started with " opts')
        
        (Thread/setDefaultUncaughtExceptionHandler
         (reify
           Thread$UncaughtExceptionHandler
           (uncaughtException [this thread throwable]
             (l/error (format "Uncaught exception %s on thread %s" throwable thread) throwable)
             (.printStackTrace throwable)))))

      ;; (= opts ::s/invalid)
      (l/error (s/explain-str ::args args)))))
