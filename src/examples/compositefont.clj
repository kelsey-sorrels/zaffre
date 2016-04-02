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
                                     
#_(def font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2))
                                     

(defn -main [& _]
  ;; render in background thread
   (let [terminal   (zgl/make-terminal [:text :rainbow]
                                       {:title "Zaffre demo"
                                        :columns 16 :rows 16
                                        :default-fg-color [250 250 250]
                                        :default-bg-color [5 5 8]
                                        :windows-font font
                                        :else-font font
                                        :icon-paths ["images/icon-16x16.png"
                                                     "images/icon-32x32.png"
                                                     "images/icon-128x128.png"]})
       event-chan   (async/merge
                      [(zat/get-key-chan terminal)
                       (zat/get-mouse-chan terminal)]
                      10)
        last-key    (atom nil)
        trail-length 6
        last-mouse-pos (atom [])
        ;; Every 10ms, set the "Rainbow" text to have a random fg color
        fx-chan     (go-loop []
                      (dosync
                        (doseq [x (range (count "Rainbow"))
                                :let [rgb (hsv->rgb (double (rand 360)) 1.0 1.0)]]
                            (zat/set-fx-fg! terminal :rainbow (inc x) 1 rgb)))
                        (zat/refresh! terminal)
                      (Thread/sleep 10)
                      (recur))
        ;; Every 33ms, draw a full frame
        render-chan (go-loop []
                      (dosync
                        (let [key-in (or @last-key \?)]
                          (zat/clear! terminal)
                          (zutil/put-string terminal :text 0 0 "Hello world")
                          (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                            (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                          (zutil/put-string terminal :text 12 0 (str key-in))
                          (zutil/put-string terminal :rainbow 1 1 "Rainbow")
                          (zutil/put-string terminal :rainbow 1 7 "Overlay")
                          (zat/put-chars! terminal :text [
                            {:c :metal       :fg [128 0 0] :bg [4 4 5] :x 12 :y 12}
                            {:c :panels      :fg [128 0 0] :bg [4 4 5] :x 13 :y 12}
                            {:c :stone       :fg [128 128 128] :bg [4 4 5] :x 14 :y 12}
                            {:c :masonry     :fg [128 128 0] :bg [4 4 5] :x 15 :y 12}
                            {:c :dark-metal  :fg [128 128 0] :bg [4 4 5] :x 16 :y 12}
                            {:c :dark-panels :fg [128 128 0] :bg [4 4 5] :x 17 :y 12}
                            {:c :grass       :fg [4 4 5]   :bg [0 128 0] :x 18 :y 12}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 1 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 2 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 3 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 4 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 5 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 6 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 7 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 8 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 9 :y 7}
                            {:c :metal       :fg [4 4 5] :bg [0 128 0] :x 10 :y 7}])
                          (doseq [[col row] @last-mouse-pos]
                            (zat/put-chars! terminal :text [{:c \* :fg [200 200 100] :bg [4 4 5] :x col :y row}]))
                          (zat/refresh! terminal)))
                          ;; ~30fps
                        (Thread/sleep 33)
                        (recur))]
    ;; get key presses in fg thread
    (loop []
      (let [new-event (async/<!! event-chan)]
        (cond
          (map? new-event)
            (let [{:keys [mouse-leave]} new-event]
              (log/info "Got mouse-event" new-event (vec @last-mouse-pos))
              (when mouse-leave
                (swap! last-mouse-pos (fn [col] (take trail-length (cons mouse-leave col)))))
              (recur))
          (char? new-event)
            (let [new-key new-event]
              (reset! last-key new-key)
              (log/info "got key" (or (str @last-key) "nil"))
              (case new-key
                \q (zat/destroy! terminal)
                nil)
              (if (= new-key :exit)
                (do
                  (async/close! fx-chan)
                  (async/close! render-chan)
                  (System/exit 0))
                ;; change font size on s/m/l keypress
                (recur))))))))


