(defproject lock-manager "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [com.pi4j/pi4j-core "1.2-SNAPSHOT"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [cider/cider-nrepl "0.15.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]
                 [com.fazecast/jSerialComm "1.3.11"]
                 [org.clojure/core.async "0.3.443"]
                 [metosin/compojure-api "2.0.0-alpha7"]
                 [ring/ring-mock "0.3.1"]
                 [clj-mqtt-component "0.1.0"]
                 [seesaw "1.4.5"]]
  :repl-options {:init-ns user}
  :main ^:skip-aot lock-manager.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev"]
                   :dependencies [[inspectable "0.2.2"]
                                  [com.stuartsierra/component.repl "0.2.0"]]}})
