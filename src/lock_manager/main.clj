(ns lock-manager.main
  (:require [clojure.tools.nrepl.server :as nrepl]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [clj-mqtt-component.core :refer [make-mqtt]]
            [com.stuartsierra.component :as comp]
            [lock-manager.card-reader.mock :refer [make-mock-card-reader]]
            [lock-manager.gps.gpsd :refer [make-gpsd]]
            [lock-manager.card-reader.serial :refer [make-serial-card-reader]]
            [lock-manager.car.mock :refer [make-car-mock]]
            [lock-manager.car.genius :refer [make-car-genius]]
            [lock-manager.core :refer [make-core]]
            [lock-manager.gpio.pi-one :refer [make-pi-one-gpio]]
            [lock-manager.gpio.mock :refer [make-mock-gpio]]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as str]
            [taoensso.timbre.appenders.core :refer [println-appender]]
            [taoensso.timbre.appenders.3rd-party.rotor :refer [rotor-appender]])
  (:gen-class))

;; Raspi /etc/rc.local
;; -------------------
;; /home/pi/umtskeeper/umtskeeper --sakisoperators "USBINTERFACE='3' OTHER='USBMODEM' USBMODEM='1199:68a3' APN='prepago.ancel' CUSTOM_APN='prepago.ancel' APN_USER='bam' APN_PASS='bam'" --sakisswitches "--sudo --console" --devicename 'telus' --log --silent --monthstart 8 --nat 'no' --httpserver &>> /home/pi/umtskeeper/error.log &
;; nohup java -jar /home/pi/lock-manager-0.1.0-standalone.jar --card-reader serial --car-id colt --mqtt-url tcp://165.227.146.60:1883 --gpio pione &

(def system nil)

(defn create-system [opts]
  (comp/system-map
   :card-reader (case (:card-reader opts)
                  "serial" (make-serial-card-reader)
                  (make-mock-card-reader {:ui true}))
   :car (comp/using (make-car-genius)
                    [:gpio])
   :core (comp/using (make-core {:car-id (:car-id opts)})
                     [:car :card-reader :mqtt])
   :gpio (case (:gpio opts)
           "pione" (make-pi-one-gpio)
           (make-mock-gpio))
   :mqtt (make-mqtt {:url (:mqtt-url opts)
                     :client-id (:car-id opts)
                     :keep-alive-interval 60
                     :on-connection-lost (fn [e] (l/warn "MQTT connection lost." e))
                     :on-connect-complete (fn [client reconnect? uri]
                                            (l/info "MQTT connection created to " uri " with id " (:car-id opts) " with reconnect set to " reconnect?))})
   :gps (comp/using (make-gpsd (:car-id opts))
                    [:mqtt])))

(s/def ::card-reader-opt (s/cat :pref #{"--card-reader"}
                                :val #{"serial" "mock"}))
(s/def ::gpio-opt (s/cat :pref #{"--gpio"}
                         :val #{"pione" "mock"}))
(s/def ::mqtt-url-opt (s/cat :pref #{"--mqtt-url"}
                             :val string?))
(s/def ::car-id-opt (s/cat :pref #{"--car-id"}
                           :val string?))
(s/def ::args (s/* (s/alt :card-reader ::card-reader-opt
                          :gpio ::gpio-opt
                          :mqtt-url ::mqtt-url-opt
                          :car-id ::car-id-opt)))

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
                   :gpio (-> opts :gpio :val)
                   :car-id (-> opts :car-id :val)
                   :mqtt-url (-> opts :mqtt-url :val)}]

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
