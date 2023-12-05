(ns examples.tileset
  (:require [zaffre.aterminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
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
  (zgl/make-terminal
    [:ui :0 :1 :2 :3]
    {:title "Zaffre demo"
     :columns 31 :rows 35
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]
     :windows-font font
     :else-font font
     :icon-paths ["images/icon-16x16.png"
                  "images/icon-32x32.png"
                  "images/icon-128x128.png"]}
    (fn [terminal]
      (let [term-pub      (zat/pub terminal)
            key-chan      (async/chan)
            mouse-chan    (async/chan 100)
            close-chan    (async/chan)
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
                            (zutil/put-string terminal :ui 2 33 (format "layer: %s" (str @current-layer)))
                            (zutil/put-string terminal :ui 16 33 (str @current-tile))
                            (zat/put-chars! terminal :ui palette)
                            (zat/put-chars! terminal :ui [{:c @current-tile :fg [255 255 255] :bg [128 128 128] :x 0 :y 13}])
                            (zat/put-chars! terminal :0 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :0)))
                            (zat/put-chars! terminal :1 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :1)))
                            (zat/put-chars! terminal :2 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :2)))
                            (zat/put-chars! terminal :3 (map (fn [[[col row] t]]
                                                               {:c t :fg [255 255 255] :bg [0 0 0] :x col :y row})
                                                             @(get layers :3)))
                            (zat/refresh! terminal))
                          ;; ~30fps
                          (Thread/sleep 33)
                          (recur))]
        (async/sub term-pub :keypress key-chan)
        (async/sub term-pub :mousedown mouse-chan)
        (async/sub term-pub :close close-chan)
        ;; get key presses in fg thread
        (go-loop []
          (let [keypress (async/<! key-chan)]
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
              \q (zat/destroy! terminal))
            (recur)))
         (go-loop []
           (let [click (async/<! mouse-chan)
                 {:keys [button state col row]} click]
            (log/info "got click" click)
            (if (< col 12)
              ;; select new tile
              (do
                (log/info "selecting tile at" [row col])
                (reset! current-tile (get-in ztiles/fantasy-map [row col])))
              ;; draw new tile
              (do
                (log/info "placing tile at" [row col])
                (swap! (get layers @current-layer) (fn [layer] (assoc layer [col row] @current-tile))))))
            (recur))
         (let [_ (async/<! close-chan)]
          (log/info "got close")
          (async/close! render-chan)
          (async/close! key-chan)
          (async/close! mouse-chan)
          (async/close! close-chan)
          (System/exit 0))))))


