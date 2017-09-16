(ns lock-manager.gpio.mock
  (:require [lock-manager.gpio.protocols :refer :all]
            [com.stuartsierra.component :as comp]
            [seesaw.core :as ss]
            [seesaw.border :as ssborder]
            [taoensso.timbre :as l]
            [clojure.spec.alpha :as s]))

(defrecord MockGpio [read-status-atom pins-widgets frame])

(def status-color {:high :red
                   :low "#ffa5ab"})

(defn make-unused-pin [id]
  (ss/label :id id :text id :border (ssborder/line-border)))

(defn make-input-pin [id label press-fn release-fn]
  (ss/toggle :text (str id " " label)
             :listen [:item-state-changed (fn [e]
                                            (if (= 1 (.getStateChange e))
                                              (press-fn id)
                                              (release-fn id)))]))
(require '[seesaw.dev :as d])

(defn make-output-pin [id label low-high]
  (ss/label :id id
            :text (str id " " label)
            :background (status-color low-high)
            :border (ssborder/line-border)))

(extend-type MockGpio
  comp/Lifecycle
  (start [this]
    (let [frame (ss/frame :title "GPIO")]
      (assoc this
             :read-status-atom (atom {})
             :pins-widgets (atom {})
             :frame frame)))
  
  (stop [this]
    (ss/dispose! (:frame this))
    this)

  GpioP

  (config-pins [{:keys [frame pins-widgets read-status-atom]} pins-config]
    (-> frame
        ss/pack!
        ss/show!)
    (reset! pins-widgets (->> pins-config
                              (map #(vector (first %) %))
                              (into (reduce #(assoc %1 %2 nil) {} (range 1 27)))
                              (map (fn [[id [_ in-out low-high label :as pdet]]]
                                     [id
                                      (cond
                                        (nil? pdet) (make-unused-pin id)
                                        (= in-out :in)  (make-input-pin id
                                                                        label
                                                                        #(swap! read-status-atom assoc % :high)
                                                                        #(swap! read-status-atom assoc % :low))
                                        (= in-out :out) (make-output-pin id label low-high))]))
                              (into (sorted-map))))
    (ss/config! frame :content (ss/grid-panel :columns 2 :items (vals @pins-widgets))))
  
  (set-pin [{:keys [pins-widgets]} pin-number high-or-low]
    (ss/config! (get @pins-widgets pin-number) :background (status-color high-or-low)))
  
  (read-pin [{:keys [read-status-atom]} pin-number]
    (get @read-status-atom pin-number)))

(defn make-mock-gpio []
  (map->MockGpio {}))
