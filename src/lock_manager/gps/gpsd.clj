(ns lock-manager.gps.gpsd
  (:require [taoensso.timbre :as l]
            [com.stuartsierra.component :as comp]
            [clj-mqtt-component.core :as mqtt]
            [geo.spatial :as spatial])
  (:import de.taimos.gpsd4java.api.IObjectListener
           [de.taimos.gpsd4java.backend GPSdEndpoint ResultParser]
           [de.taimos.gpsd4java.types ATTObject DeviceObject DevicesObject SKYObject TPVObject]
           de.taimos.gpsd4java.types.subframes.SUBFRAMEObject))

(defrecord Gpsd [end-point mqtt car-id])

(extend-type Gpsd
  comp/Lifecycle
  (start [this]
    (let [end-point (GPSdEndpoint. "localhost" 2947 (ResultParser.))
          last-location-report-point (atom nil)]
      (.addListener end-point
                    (reify IObjectListener
                      
                      (^void handleTPV [this ^TPVObject tpv]
                       (let [location-point (spatial/spatial4j-point (.getLatitude tpv) (.getLongitude tpv))]
                         (when (> (spatial/distance location-point @last-location-report-point)
                                  5)
                           (mqtt/publish (:mqtt this) (str (:car-id this) "/position") {:latitude (.getLatitude tpv)
                                                                                        :longitude (.getLongitude tpv)})
                           (l/debug "Reporting a position change to " {:latitude (.getLatitude tpv)
                                                                       :longitude (.getLongitude tpv)})
                           (reset! last-location-report-point location-point))))
                      
                      (^void handleSKY [this ^SKYObject sky])
                      
                      (^void handleSUBFRAME [this ^SUBFRAMEObject subframe])
                      
                      (^void handleATT [this ^ATTObject att])
                      
                      (^void handleDevice [this ^DeviceObject device])
                      
                      (^void handleDevices [this ^DevicesObject devices])))

      (.start end-point)
      (.watch end-point true true)
      
      (assoc this
             :end-point end-point)))
  
  (stop [this]
    (.stop (:end-point this))
    (assoc this :end-point nil)))

(defn make-gpsd [car-id]
  (map->Gpsd {:car-id car-id}))
