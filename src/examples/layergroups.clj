(ns examples.layergroups
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
           (zaffre.font CompositeFont CP437Font TileSet TTFFont))
  (:gen-class))

(defn hsv->rgb [h s v]
  (let [c (* v s)
        x (* c (- 1.0 (Math/abs (double (dec (mod (/ h 60.0) 2.0))))))]
    (mapv (comp int (partial * 255))
      (cond
        (< h  60) [c x 0]
        (< h 120) [x c 0]
        (< h 180) [0 c x]
        (< h 240) [0 x c]
        (< h 300) [x 0 c]
        (< h 360) [c 0 x]))))

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

;; Use zfont/construct to download and build the font now
;; If zfont/construct was ommitted, each alter-group-font! call would result in an HTTP GET of the font image
(def fg-font-a (zfont/construct (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true)))
(def fg-font-b (zfont/construct (CP437Font. "http://dwarffortresswiki.org/images/f/ff/CGA8x8thin.png" :green 1 true)))
                                     
(defn background-chars []
  (for [x (range 1 11)
        y (range 5 10)]
    {:c :metal       :fg [4 4 5] :bg [0 128 0] :x x :y y}))

(defn -main [& _]
  ;; render in background thread
   (zgl/create-terminal
     {:background {
        :layers [:text]
        :columns 16
        :rows 16
        :pos [0 0]
        :font (constantly font)}
      :foreground {
        :layers [:overlay]
        :columns 32
        :rows 32
        :pos [8 8]
        :font (constantly fg-font-a)}
     }
     {:title "Zaffre demo"
      :screen-width (* 16 16)
      :screen-height (* 16 16)
      :default-fg-color [250 250 250]
      :default-bg-color [5 5 8]}
     (fn [terminal]
       (let [last-key    (atom nil)
             fg-font     (atom :fg-font-a)]
             ;; Every 33ms, draw a full frame
         (zat/do-frame terminal 33
           (let [key-in (or @last-key \?)]
             (zutil/put-string terminal :text 0 0 "Hello world")
             (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
               (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
             (zutil/put-string terminal :text 12 0 (str key-in))
             (zutil/put-string terminal :overlay 2 7 "Overlay")
             (zutil/put-string terminal :overlay 2 8 "Overlay")
             (zat/put-chars! terminal :text
               (background-chars))
             (zat/alter-group-pos!
               terminal
               :foreground
               (fn [_]
                 (let [t (System/currentTimeMillis)
                       y (int (* 32 (Math/sin (/ t 2000))))]
                   [0 y])))))
         (zevents/add-event-listener terminal :keypress
           (fn [new-key]
             (reset! last-key new-key)
             (log/info "got key" (or (str @last-key) "nil"))
             (case new-key
               \f (case @fg-font
                    :fg-font-a (do
                                 (try
                                   (do
                                     (reset! fg-font :fg-font-b)
                                     (zat/alter-group-font! terminal :foreground (constantly fg-font-b)))
                                   (catch Exception e
                                     (log/error e)))
                                 (log/info "Done altering font"))
                    :fg-font-b (do
                                 (do
                                   (reset! fg-font :fg-font-a)
                                   (zat/alter-group-font! terminal :foreground (constantly fg-font-a)))
                                 (log/info "Done altering font"))
                    nil)
               \q (zat/destroy! terminal)
               nil)))))))
