(ns examples.compositefont
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
           (zaffre.font CompositeFont CP437Font TileSet TTFFont)))

(def one-bit-map [[:metal      :panels      :stone        :masonry         :dark-metal :dark-panels  :grass       :tree]
                  [:water-nw   :water-n     :water-ne     :column          :parapet    :parapet-flag :sign        :palm]
                  [:water-w    :water       :water-e      :fence           :gate       :dead-tree    :tombstone1  :tombstone2]
                  [:water-sw   :water-s     :water-se     :black           :pine-tree  :cave         :cave-nw     :cave-ne]
                  [:water-f-se :water-f-sw  :masonry-top  :wall-top        :gravel     :mountain     :cave-sw     :cave-se]
                  [:water-f-se :water-f-nw  :window       :wall            :robot      :lever-left   :lever-right :key]
                  [:dark-water :locked-door :open-door    :masonry-parapet :window     :ladder       :stairs-up   :stairs-down]
                  [:cactus     :skull       :boulder      :crate           :barrel     :chest-closed :chest-open  :blank]])

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

(defn map->tile->transparent [tilemap]
  (zipmap (flatten tilemap) (repeat false)))

(def font (CompositeFont. [(CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true)
                           (TileSet. "http://opengameart.org/sites/default/files/tileset_1bit.png" :green 16 16 0
                                     (map->tile->col-row one-bit-map)
                                     (map->tile->transparent one-bit-map))]))
                                     
(defn -main [& _]
  ;; render in background thread
   (zgl/create-terminal
     {:app {
       :layers [:text :overlay]
       :columns 16 :rows 16
       :pos [0 0]
       :font (constantly font)}}
     {:title "Zaffre demo"
      :screen-width (* 16 16)
      :screen-height (* 16 16)
      :default-fg-color [250 250 250]
      :default-bg-color [5 5 8]
      :icon-paths ["images/icon-16x16.png"
                   "images/icon-32x32.png"
                   "images/icon-128x128.png"]}
     (fn [terminal]
       (let [last-key    (atom nil)
             ;; Every 33ms, draw a full frame
             render-chan (zat/do-frame terminal 33
                           (let [key-in (or @last-key \?)]
                             (zutil/put-string terminal :text 0 0 "Hello world")
                             (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                               (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                             (zutil/put-string terminal :text 12 0 (str key-in))
                             (zutil/put-string terminal :overlay 2 7 "Overlay")
                             (zat/put-chars! terminal :text
                               (for [x (range 1 11)
                                     y (range 5 10)]
                                 {:c :metal       :fg [4 4 5] :bg [0 128 0] :x x :y y}))))]
         (zevents/add-event-listener terminal :keypress
           (fn [new-key]
             (reset! last-key new-key)
             (log/info "got key" (or (str @last-key) "nil"))
             (case new-key
               \q (zat/destroy! terminal)
               nil)))
         (zevents/wait-for-close terminal [render-chan])))))
