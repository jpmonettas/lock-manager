(ns lock-manager.main
  (:require [clojure.tools.nrepl.server :as nrepl]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [com.stuartsierra.component :as comp]
            [lock-manager.card-reader.mock :refer [make-mock-card-reader]]
            [lock-manager.card-reader.rc-522 :refer [make-rc522-card-reader]]
            [lock-manager.car.genius :refer [make-car-genius]]
            [lock-manager.car.mock :refer [make-car-mock]]
            [lock-manager.core :refer [make-core]]
            [lock-manager.web-server :refer [make-web-server]]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as str])
  (:gen-class))

(def system nil)

(defn create-system [opts]
  (comp/system-map
   :card-reader (case (get-in [:card-reader :val] opts)
                  "rc522" (make-rc522-card-reader)
                  (make-mock-card-reader))
   :car (case (get-in [:car :val] opts)
          "genius" (make-car-genius)
          (make-car-mock))
   :web-server (make-web-server)
   :core (comp/using (make-core)
                     [:car :card-reader :web-server])))

(s/def ::car-opt (s/cat :pref #{"--car"}
                        :val #{"genius" "mock"}))
(s/def ::card-reader-opt (s/cat :pref #{"--card-reader"}
                                :val #{"rc522" "mock"}))
(s/def ::args (s/* (s/alt :car ::car-opt
                          :card-reader ::card-reader-opt)))

(defn start-system [opts]
  (alter-var-root #'system (fn [s] (comp/start (create-system (into {} opts))))))

(defn stop-system []
  (alter-var-root #'system (fn [s]
                             (when s (comp/stop s)))))

(defn -main
  [& args]

  (let [opts (s/conform ::args args)]
    (if (not= opts ::s/invalid)
      (do
        (Thread/setDefaultUncaughtExceptionHandler
         (reify
           Thread$UncaughtExceptionHandler
           (uncaughtException [this thread throwable]
             (l/error (format "Uncaught exception %s on thread %s" throwable thread)))))
        
        (start-system opts)
        
        (l/info "System started.")
        
        (nrepl/start-server :handler cider-nrepl-handler
                            :port 7778
                            :bind "0.0.0.0")
        (l/info "Nrepl server started."))

      ;; (= opts ::s/invalid)
      (l/error (s/explain-str ::args args)))))
