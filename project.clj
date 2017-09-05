(defproject lock-manager "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [com.pi4j/pi4j-core "1.2-SNAPSHOT"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [cider/cider-nrepl "0.15.0"]
                 [http-kit "2.2.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [com.taoensso/timbre "4.10.0"]
                 [re-frame "0.10.1"]
                 [org.clojure/test.check "0.10.0-alpha2"]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]
                 [clj-serial "2.0.4-SNAPSHOT"]
                 [org.clojure/core.async "0.3.443"]]
  :java-source-paths ["src-java"]
  :main ^:skip-aot lock-manager.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
