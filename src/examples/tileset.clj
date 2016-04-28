(ns examples.tileset
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.events :as zevents]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
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


#_(def font (CompositeFont. [ztiles/pastiche-16x16
                           ztiles/two-bit-tileset]))

#_(def font (CompositeFont. [ztiles/pastiche-16x16
                           ztiles/d-roguelike]))

(def font (CompositeFont. [ztiles/pastiche-16x16
                           ztiles/fantasy]))

(def palette (for [[row line] (map-indexed vector ztiles/fantasy-map)
                   [col id]   (map-indexed vector line)]
               {:c id :fg [255 255 255] :bg [0 0 0] :x col :y row}))

(defn -main [& _]
  ;; render in background thread
  (zgl/create-terminal
    {:app {
      :layers  [:ui :0 :1 :2 :3]
      :columns 32
      :rows    32
      :pos     [0 0]
      :font    (constantly font)}}
    {:title            "Zaffre demo"
     :screen-width     (* 33 16)
     :screen-height    (* 35 16)
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]}
    (fn [terminal]
      (let [term-pub      (zat/pub terminal)
            ;key-chan      (async/chan)
            mouse-chan    (async/chan 100)
            layers        {:0 (atom {})
                           :1 (atom {})
                           :2 (atom {})
                           :3 (atom {})}
            current-layer (atom :0)
            current-tile  (atom (ffirst ztiles/fantasy-map))
            ;; Every 33ms, draw a full frame
            render-chan (go-loop []
                          (dosync
                            (zat/clear! terminal)
                            (zutil/put-string terminal :ui 2 30 (format "layer: %s" (str @current-layer)))
                            (zutil/put-string terminal :ui 16 30 (str @current-tile))
                            (zat/put-chars! terminal :ui palette)
                            (zat/put-chars! terminal :ui [{:c @current-tile :fg [255 255 255] :bg [128 128 128] :x 0 :y 30}])
                            (zat/put-chars! terminal :0 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :0)))
                            (zat/put-chars! terminal :1 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :1)))
                            (zat/put-chars! terminal :2 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :2)))
                            #_(zat/put-chars! terminal :3 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :3)))
                            (zat/refresh! terminal))
                          ;; ~30fps
                          (Thread/sleep 33)
                          (recur))]
        ;(async/sub term-pub :keypress key-chan)
        ;(async/sub term-pub :click mouse-chan)
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
                (when-let [tile (get-in ztiles/fantasy-map [row col])]
                  (reset! current-tile tile)))
              ;; draw new tile
              (do
                (log/info "placing tile at" [row col])
                (swap! (get layers @current-layer) (fn [layer] (assoc layer [col row] @current-tile)))))))
        (zevents/wait-for-close terminal [render-chan mouse-chan])))))
