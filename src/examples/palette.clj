(ns examples.palette
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.imageutil :as zimg]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.events :as zevents]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
           (zaffre.font CompositeFont CP437Font TileSet TTFFont)))

(def tileset-bw
  (zfont/scale
    (zfont/tileset
      "dev-resources/Tileset_BW.png"
      :alpha 8 8 0
      (zfont/dynamic-object "dev-resources/Tileset_BW.edn")
      (constantly true)
      (zfont/dynamic-pal "dev-resources/Tileset_BW.pal"))
    2))

(def font (CompositeFont. [@ztiles/pastiche-16x16
                           tileset-bw]))

(defn palette-chars
  [glyph-graphics palette-offset]
  (for [[row line] (map-indexed vector (zfont/character-layout glyph-graphics))
        [col id]   (map-indexed vector line)]
    {:c id :fg [255 255 255] :bg [0 0 0] :x col :y row :palette-offset palette-offset}))

(def color-table-chars
  (->>
    (for [row (range 3)
        col (range 4)]
      {:c :dark-trans :fg [0 0 0] :bg [0 0 0] :x col :y (+ row 28)})
    (map-indexed (fn [i c] (assoc c :palette-offset i)))))
   

(log/info font)
;(log/info "palette" (vec palette))

(defn -main [& _]
  ;; render in background thread
  (zgl/create-terminal
    [{:id :app
      :layers  [:ui :0 :1 :2 :3]
      :columns 32
      :rows    32
      :pos     [0 0]
      :font    (constantly font)}]
    {:title            "Zaffre demo"
     :screen-width     (* 32 16)
     :screen-height    (* 33 16)
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]}
    (fn [terminal]
      (let [term-pub      (zat/pub terminal)
            ;key-chan      (async/chan)
            ;mouse-chan    (async/chan 100)
            layers        {:0 (atom {})
                           :1 (atom {})
                           :2 (atom {})
                           :3 (atom {})}

            font-graphics (atom (zfont/glyph-graphics font))
            palette-graphics (atom (zfont/glyph-graphics tileset-bw))
            current-layer (atom :0)
            current-tile  (atom (let [tile-names (zfont/character-layout @palette-graphics)]
                                  (log/info "tile-names" tile-names)
                                  (ffirst tile-names)))
            palette-offset (atom 0)]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33
          (when (or (zfont/dirty? tileset-bw)
                    (zfont/dirty? font))
            (reset! palette-graphics (zfont/glyph-graphics tileset-bw))
            (reset! font-graphics (zfont/glyph-graphics font))
            (zat/alter-group-font! terminal :app (constantly @font-graphics)))
          (zat/put-chars! terminal :ui (palette-chars @palette-graphics @palette-offset))
          (zat/put-chars! terminal :ui [{:c @current-tile :fg [255 255 255] :bg [0 0 0] :x 0 :y 26 :palette-offset @palette-offset}])
          (zutil/put-string terminal :ui 2 26 (format "layer: %s" (str @current-layer)))
          (when @current-tile
            (zutil/put-string terminal :ui 16 26 (str @current-tile)))
          (zat/put-chars! terminal :ui color-table-chars)
          (log/info (vec color-table-chars))
          (zat/put-chars! terminal :0 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0 0] :x col :y row :palette-offset @palette-offset})
                                           @(get layers :0)))
          (zat/put-chars! terminal :1 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0 0] :x col :y row :palette-offset @palette-offset})
                                           @(get layers :1)))
          (zat/put-chars! terminal :2 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0 0] :x col :y row :palette-offset @palette-offset})
                                           @(get layers :2)))
          (zat/put-chars! terminal :3 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0 0] :x col :y row :palette-offset @palette-offset})
                                           @(get layers :3))))
        (zevents/add-event-listener terminal :keypress 
          (fn [keypress]
            (log/info "got keypress" keypress)
            ;; change font size on s/m/l keypress
            (case keypress
              \+ (swap! current-layer (fn [current-layer]
                                        (case current-layer
                                          :0 :1
                                          :1 :2
                                          :2 :3
                                          :3 :3)))
              \- (swap! current-layer (fn [current-layer]
                                        (case current-layer
                                          :0 :0
                                          :1 :0
                                          :2 :1
                                          :3 :2)))
              \q (zat/destroy! terminal)
              nil)))
        (zevents/add-event-listener terminal :click
          (fn [{:keys [button state col row] :as click}]
            (log/info "got click" click)
            (if (< col 12)
              (if (< row 27)
                ;; select new tile
                (let [tile-names (zfont/character-layout @palette-graphics)]
                  (log/info "selecting tile at" [row col])
                  (when-let [tile (get-in tile-names [row col])]
                    (reset! current-tile tile)))
                (let [offset (* 4 (- row 28))]
                  (log/info "palette-offset" offset)
                  (reset! palette-offset offset)))
              ;; draw new tile
              (do
                (log/info "placing tile at" [row col])
                (swap! (get layers @current-layer) (fn [layer] (assoc layer [col row] @current-tile)))))))))))
