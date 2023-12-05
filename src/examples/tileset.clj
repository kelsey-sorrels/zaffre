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


(def font (CompositeFont. [ztiles/pastiche-16x16
                           ztiles/two-bit-tileset]))

(def palette (for [[row line] (map-indexed vector ztiles/two-bit-map)
                   [col id]   (map-indexed vector line)]
               {:c id :fg [255 255 255] :bg [0 0 0] :x col :y row}))

(defn -main [& _]
  ;; render in background thread
   (let [terminal     (zgl/make-terminal [:ui :0 :1 :2 :3]
                                         {:title "Zaffre demo"
                                          :columns 48 :rows 25
                                          :default-fg-color [250 250 250]
                                          :default-bg-color [5 5 8]
                                          :windows-font font
                                          :else-font font
                                          :icon-paths ["images/icon-16x16.png"
                                                       "images/icon-32x32.png"
                                                       "images/icon-128x128.png"]})
        layers        {:0 (atom [])
                       :1 (atom [])
                       :2 (atom [])
                       :3 (atom [])}
        current-layer (atom :0)
        ;; Every 33ms, draw a full frame
        render-chan (go-loop []
                      (dosync
                        (zat/clear! terminal)
                        (zutil/put-string terminal :ui 0 24 (format "layer: %s" (str @current-layer)))
                        (zat/put-chars! terminal :ui palette)
                        (zat/refresh! terminal))
                        ;; ~30fps
                        (Thread/sleep 33)
                        (recur))]
    ;; get key presses in fg thread
    (loop []
      (let [new-key (async/<!! (zat/get-key-chan terminal))]
        (log/info "got key" (or (str new-key) "nil"))
        ;; change font size on s/m/l keypress
        (case new-key
          \q (zat/destroy! terminal)
          nil)
        (if (= new-key :exit)
          (do
            (async/close! render-chan)
            (System/exit 0))
          (recur))))))


