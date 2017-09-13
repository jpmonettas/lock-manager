(ns dev
  (:require [lock-manager.main :as main]
            [clojure.tools.namespace.repl :refer [refresh]]
            [lock-manager.car.protocols :refer :all]
            [lock-manager.card-reader.protocols :refer :all]
            [taoensso.timbre :as l]
            [inspectable.repl :as ir]
            [clojure.spec.alpha :as s]
            [com.stuartsierra.component.repl :refer [set-init system stop]]))

(defn reset []
  (com.stuartsierra.component.repl/reset))

(set-init (fn [_] (main/create-system {:car "genius" :card-reader "mock" :gpio "mock"})))

(defn car-cmp [] (:car system))
(defn card-r-cmp [] (:card-reader system))

(Thread/setDefaultUncaughtExceptionHandler
         (reify
           Thread$UncaughtExceptionHandler
           (uncaughtException [this thread throwable]
             (ir/repl-caught throwable)
             (l/error (format "!!!! Uncaught exception %s on thread %s" throwable thread) throwable)
             (.printStackTrace throwable))))
