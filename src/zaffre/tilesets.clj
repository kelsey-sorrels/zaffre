(ns zaffre.tilesets
  (:require [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (zaffre.font CP437Font TileSet TTFFont)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn map->tile->col-row [tilemap]
  (reduce (fn [m [t col row]]
            (assoc m t [col row]))
          {}
          (mapcat concat
            (map-indexed (fn [row line]
                           (map-indexed (fn [col t]
                                          [t col row])
                                        line))
                         tilemap))))

(defn map->tile->transparent [tilemap v]
  (zipmap (flatten tilemap) (repeat v)))

;; 1-bit tileset by Clint Bellanger
;; CC-BY 3.0
;; http://opengameart.org/content/tileset-1bit-color
(def one-bit-map [[:metal      :panels      :stone        :masonry         :dark-metal :dark-panels  :grass       :tree]
                  [:water-nw   :water-n     :water-ne     :column          :parapet    :parapet-flag :sign        :palm]
                  [:water-w    :water       :water-e      :fence           :gate       :dead-tree    :tombstone1  :tombstone2]
                  [:water-sw   :water-s     :water-se     :black           :pine-tree  :cave         :cave-nw     :cave-ne]
                  [:water-f-se :water-f-sw  :masonry-top  :wall-top        :gravel     :mountain     :cave-sw     :cave-se]
                  [:water-f-se :water-f-nw  :window       :wall            :robot      :lever-left   :lever-right :key]
                  [:dark-water :locked-door :open-door    :masonry-parapet :window     :ladder       :stairs-up   :stairs-down]
                  [:cactus     :skull       :boulder      :crate           :barrel     :chest-closed :chest-open  :blank]])

(def one-bit-tileset (TileSet. "http://opengameart.org/sites/default/files/tileset_1bit.png" :green 16 16 0
                               (map->tile->col-row one-bit-map)
                               (map->tile->transparent one-bit-map false)))

;; 2-bit tileset by Muziak
;; http://pixeljoint.com/pixelart/71895.htm
(def two-bit-map [[:tree-nw :tree-ne :small-tree :stump-w :stump-e :roof-nw :roof-n :roof-ne :factory-roof-nw :factory-roof-n :factory-foor-ne :palette]
                  [:tree-sw :tree-se :hedge-nw   :hedge-n :hedge-ne :roof-sw :roof-s :roof-se :door-w :door :door-e :empty]
                  [:fence-w :fence :fence-e      :empty   :turf     :window-w :window :window-e :wall-w :wall :wall-e :empty]
                  [:water-nw :water-n :water-ne :se :sw  :flowers :vine-nw :vine-ne :hedge-e :fence-nw :fence-ne :empty]
                  [:water-w  :land   :water-e :ne :nw :pattern-1 :vine-sw :vine-se :hedge-w :fence-v :empty :fence-v :empty]
                  [:water-sw :water-s :water-se :water :turf-2 :rocks :hedge-sw :hedge-se :dock-n :fence-sw :fence-se :empty]
                  [:fruit-tree-nw :fruit-tree-ne :diamond :pattern-2 :pattern-3 :pattern-4 :pattern-5 :empty :dock-v :fence-sw-2 :fence-se-2 :empty]
                  [:fruit-tree-sw :fruit-tree-se :waves :lilly-pad :? :dock-w :dock-h :dock-e :dock-s :stairs-left-w :stairs-left-e :empty]
                  [:pattern-6 :stairs-v :empty :? :? :? :? :barrel :signpost-top :stairs-right-w :stairs-right-e :empty]
                  [:pattern-7 :empty :? :empty :? :? :? :empty :signpost-bottom :empty :empty]
                  [:pattern-8 :pattern-9 :empty :? :? :? :? :? :? :? :? :empty]
                  [:pine-nw :pine-ne :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
                  [:pine-sw :pine-se :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]])

(def two-bit-tileset (TileSet. "http://pixeljoint.com/files/icons/full/tileset__r1752392111.png" :alpha 16 16 1
                               (map->tile->col-row two-bit-map)
                               (map->tile->transparent two-bit-map true)))

;; CP437 tilesets from
;; http://dwarffortresswiki.org/Tileset_repository
(def pastiche-8x8 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true))
(def pastiche-16x16 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true))


