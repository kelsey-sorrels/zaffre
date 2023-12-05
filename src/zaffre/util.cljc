(ns zaffre.util
  (:require [zaffre.terminal :as zat]
            clojure.set)
  (:import (zaffre.terminal Terminal)))

(defn next-pow-2 [v]
  (int (Math/pow 2 (Math/ceil (/ (Math/log v) (Math/log 2))))))

(defn put-string
  ([^ATerminal screen layer-id x y string]
   (put-string screen layer-id (int (Math/ceil x)) (int (Math/ceil y)) string [255 255 255] [0 0 0] #{}))
  ([^ATerminal screen layer-id x y string fg bg]
   (put-string screen layer-id (int (Math/ceil x)) (int (Math/ceil y)) string fg bg #{}))
  ([^ATerminal screen layer-id x y string fg bg styles]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (let [characters (map-indexed (fn [i c] {:c  c
                                            :fg fg
                                            :bg bg
                                            :x  (+ x i)
                                            :y  y})
                                 string)]
     (zat/put-chars! screen layer-id characters))))
