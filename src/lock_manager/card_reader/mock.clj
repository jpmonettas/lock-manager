(ns lock-manager.card-reader.mock
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators :as tcg]
            [com.stuartsierra.component :as comp]
            [taoensso.timbre :as l]
            [lock-manager.card-reader.protocols :refer :all]
            [clojure.core.async :as async]
            [seesaw.core :as ss]))

(defrecord MockCardReader [call-backs frame ui])

(extend-type MockCardReader

  comp/Lifecycle

  (start [this]
    (let [call-backs (atom {})
          frame (ss/frame :title "Card reader"
                          :content (ss/flow-panel
                                    :items (let [text-input (ss/text :id :rfid :text "7564F8C2")]
                                             [text-input
                                              (ss/toggle :text "Toggle to swap"
                                                         :listen [:item-state-changed (fn [e]
                                                                                        (if (= 1 (.getStateChange e))
                                                                                          (when-let [on-reader (get @call-backs :on-reader)]
                                                                                            (on-reader (ss/text text-input)))
                                                                                          (when-let [off-reader (get @call-backs :off-reader)]
                                                                                            (off-reader (ss/text text-input)))))])])))]
      (l/info "[MockCardReader] component started")
      (when (:ui this)
        (-> frame
            ss/pack!
            ss/show!))
      (assoc this
             :call-backs call-backs
             :frame frame)))

  (stop [this]
    (l/info "[MockCardReader] component stopped")
    (when-let [frame (:frame this)] (ss/dispose! frame))
    this)
  

  CardReaderP
  
  (register-card-on-reader-fn [this f]
    (swap! (:call-backs this) assoc :on-reader f))

  (register-card-off-reader-fn [this f]
    (swap! (:call-backs this) assoc :off-reader f)))

(defn make-mock-card-reader [{:keys [ui]}]
  (map->MockCardReader {:ui ui}))

(defn simulate-card-on [card-reader-cmp tag-id]
  (when-let [on-reader (get @(:call-backs card-reader-cmp) :on-reader)]
    (on-reader tag-id)))

(defn simulate-card-off [card-reader-cmp tag-id]
  (when-let [off-reader (get @(:call-backs card-reader-cmp) :off-reader)]
     (off-reader tag-id)))


