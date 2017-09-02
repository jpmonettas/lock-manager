(ns lock-manager.web-server
  (:require [com.stuartsierra.component :as comp]
            [org.httpkit.server :as httpkit-server]
            [taoensso.timbre :as l]))

(defprotocol WebServerP
  (register-add-tag-call-back [_ f])
  (register-list-tags-call-back [_ f])
  (register-rm-tag-call-back [_ f]))

(defrecord WebServer [server call-backs])

(defn build-handler [call-backs-a]
  (fn [req]
    (when-let [list-tags (:list-tags @call-backs-a)]
     {:status 200
      :body (str (list-tags))})))

(extend-type WebServer

  comp/Lifecycle

  (start [this]
    (let [call-backs (atom {})
          http-server (httpkit-server/run-server (-> (build-handler call-backs))
                                                 {:port 1234})]
      (l/info "Started WebServer component (port: 1234)")
     (assoc this
            :call-backs call-backs
            :server http-server)))
  
  (stop [this]
    ((:server this))
    (l/info "Stopped WebServer component")
    (dissoc this :server :call-backs))

  WebServerP
  
  (register-add-tag-call-back [this f]
    (swap! (:call-backs this) assoc :add-tag f))
  
  (register-list-tags-call-back [this f]
    (swap! (:call-backs this) assoc :list-tags f))
  
  (register-rm-tag-call-back [this f]
    (swap! (:call-backs this) assoc :rm-tag f)))

(defn make-web-server []
  (map->WebServer {}))
