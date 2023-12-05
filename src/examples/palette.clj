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

; 13x23
(def tile-names [
  [:ul-cover :u-cover :ur-cover :ul-dark-cover :u-dark-cover :ur-dark-cover :ul-light-cover :u-light-cover :ur-light-cover :? :? :? :? ]
  [:l-cover :cover :r-cover :l-dark-cover :dark-cover :r-dark-cover :l-light-cover :light-cover :r-light-cover :? :? :? :? ]
  [:bl-color :b-cover :br-cover :bl-dark-cover :b-dark-cover :br-dark-cover :bl-light-cover :blight-cover :br-light-cover :? :? :? :? ]
  [:ul-cover-bg :u-cover-bg :ur-cover-bg :ul-dark-cover-bg :u-dark-cover-bg :ur-dark-cover-bg :ul-light-cover-bg :u-light-cover-bg :ur-light-cover-bg :? :? :? :? ]
  [:l-cover-bg :cover-bg :r-cover-bg :l-dark-cover-bg :dark-cover-bg :r-dark-cover-bg :l-light-cover-bg :light-cover-bg :r-light-cover-bg :? :? :? :? ]
  [:bl-color-bg :b-cover-bg :br-cover-bg :bl-dark-cover-bg :b-dark-cover-bg :br-dark-cover-bg :bl-light-cover-bg :blight-cover-bg :br-light-cover-bg :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
  [:? :? :? :? :? :? :? :? :? :? :? :? :? ]
])

(def color-table
  [[[  0  25  35   0]
    [ 99 123 103 255]
    [ 28  69  69 255]
    [156 175 131 255]
    [209 213 201 255]
    [209 213 201 255]
    [209 213 201 255]]])

(def tileset-bw
  (zfont/tileset
    "dev-resources/Tileset_BW.png"
    :alpha 8 8 0 2
    (ztiles/map->tile->col-row tile-names)
    (ztiles/map->tile->transparent tile-names true)
    color-table))

(def font (CompositeFont. [ztiles/pastiche-16x16
                           tileset-bw]))

      

(def palette (for [[row line] (map-indexed vector tile-names)
                   [col id]   (map-indexed vector line)]
               {:c id :fg [255 255 255] :bg [0 0 0] :x col :y row :palette-index 1}))

(log/info font)
(log/info "palette" (vec palette))

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
            current-layer (atom :0)
            current-tile  (atom (ffirst tile-names))]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33
          (zutil/put-string terminal :ui 2 30 (format "layer: %s" (str @current-layer)))
          (when @current-tile
            (zutil/put-string terminal :ui 16 30 (str @current-tile)))
          (zat/put-chars! terminal :ui palette)
          (zat/put-chars! terminal :ui [{:c @current-tile :fg [255 255 255] :bg [128 128 128] :x 0 :y 30 :palette-index 1}])
          (zat/put-chars! terminal :0 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row :palette-index 1})
                                           @(get layers :0)))
          (zat/put-chars! terminal :1 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row :palette-index 1})
                                           @(get layers :1)))
          (zat/put-chars! terminal :2 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row :palette-index 1})
                                           @(get layers :2)))
          (zat/put-chars! terminal :3 (map (fn [[[col row] t]]
                                             {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row :palette-index 1})
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
              ;; select new tile
              (do
                (log/info "selecting tile at" [row col])
                (when-let [tile (get-in tile-names [row col])]
                  (reset! current-tile tile)))
              ;; draw new tile
              (do
                (log/info "placing tile at" [row col])
                (swap! (get layers @current-layer) (fn [layer] (assoc layer [col row] @current-tile)))))))))))
