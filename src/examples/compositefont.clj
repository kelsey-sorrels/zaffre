(ns examples.compositefont
  (:require [zaffre.aterminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.aterminal ATerminal)
           (zaffre.font CompositeFont CP437Font TileSet TTFFont)))

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
                                     
(defn -main [& _]
  ;; render in background thread
   (zgl/make-terminal
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
       (let [term-pub    (zat/pub terminal)
             key-chan    (async/chan)
             close-chan  (async/chan)
             last-key    (atom nil)
             ;; Every 33ms, draw a full frame
             render-chan (go-loop []
                           (dosync
                             (let [key-in (or @last-key \?)]
                               (zat/clear! terminal)
                               (zutil/put-string terminal :text 0 0 "Hello world")
                               (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                                 (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                               (zutil/put-string terminal :text 12 0 (str key-in))
                               (zutil/put-string terminal :overlay 2 7 "Overlay")
                               (zat/put-chars! terminal :text
                                 (for [x (range 1 11)
                                       y (range 5 10)]
                                   {:c :metal       :fg [4 4 5] :bg [0 128 0] :x x :y y}))
                               (zat/refresh! terminal)))
                               ;; ~30fps
                             (Thread/sleep 33)
                             (recur))]
         (async/sub term-pub :keypress key-chan)
         (async/sub term-pub :close close-chan)
         (go-loop []
           (let [new-key (async/<! key-chan)]
             (reset! last-key new-key)
             (log/info "got key" (or (str @last-key) "nil"))
             (case new-key
               \q (zat/destroy! terminal)
               nil)
               ;; change font size on s/m/l keypress
             (recur)))
         (let [_ (async/<! close-chan)]
           (async/close! render-chan)
           (System/exit 0))))))
