(ns examples.resizing
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.events :as zevents]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
           (zaffre.font CP437Font TTFFont)))

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

(def small-font-fn (constantly (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green true) 1)))
(def medium-font-fn (constantly (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green true) 2)))
(def large-font-fn (constantly (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green true) 3)))

(defn size->width [size]
  (case size
    :small  (* 16 8 1)
    :medium (* 16 8 2)
    :large  (* 16 8 3)))

(defn size->height [size]
  (case size
    :small  (* 16 8 1)
    :medium (* 16 8 2)
    :large  (* 16 8 3)))

(defn -main [& _]
  ;; render in background thread
  (zgl/create-terminal
    [{:id :app
       :layers [:text :rainbow]
       :columns 16
       :rows 16
       :pos [0 0]
       :font small-font-fn}]
    {:title "Zaffre demo"
     :screen-width (size->width :medium)
     :screen-height (size->height :medium)
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]}
    (fn [terminal]
      (let [term-pub    (zat/pub terminal)
            fullscreen-sizes (zat/fullscreen-sizes terminal)
            last-size   (atom :medium)
            last-key    (atom nil)
            swidth      (atom (* 16 16))
            sheight     (atom (* 16 16))]
            ;; Every 10ms, set the "Rainbow" text to have a random fg color
            ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33 [:text :rainbow]
          (let [key-in (or @last-key \?)]
            (zutil/put-string terminal :text 0 0 "Hello world")
            (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
              (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
            (zutil/put-string terminal :text 12 0 (str key-in))
            (zutil/put-string terminal :rainbow 1 1 "Rainbow")))
        #_(go-loop []
          (dosync
            (doseq [x (range (count "Rainbow"))
                    :let [rgb (hsv->rgb (double (rand 360)) 1.0 1.0)]]
                (zat/set-fg! terminal :rainbow (inc x) 1 rgb)))
            (zat/refresh! terminal)
          (Thread/sleep 10)
          (recur))
        (log/info "Fullscreen sizes" fullscreen-sizes)
        (zevents/add-event-listener terminal :font-change
          (fn [{:keys [character-width character-height] :as ev}]
            (log/info "Got :font-change" character-width character-height ev)
            #_(zat/set-window-size! terminal {:width (* @srows 16)  :height (* @scols 16)})
            #_(zat/set-window-size! terminal {:width  (size->width @last-size)
                                            :height (size->height @last-size)})))
        ;; get key presses in fg thread
        (zevents/add-event-listener terminal :keypress
          (fn [new-key]
            (reset! last-key new-key)
            (log/info "got key" (or (str @last-key) "nil"))
            ;; change font size on s/m/l keypress
            ;; \w for windowed mode, and \f for fullscreen
            (case new-key
              \s (do
                   (reset! last-size :small)
                   (zat/alter-group-font! terminal :app
                     small-font-fn))
              \m (do
                   (reset! last-size :medium)
                   (zat/alter-group-font! terminal :app
                      medium-font-fn))
              \l (do
                   (reset! last-size :large)
                   (zat/alter-group-font! terminal :app
                     large-font-fn))
              \+ (do
                   (swap! swidth (partial + 16))
                   (swap! sheight (partial + 16))
                   (zat/set-window-size! terminal {:width @swidth  :height @sheight}))
              \- (do
                   (swap! swidth (partial + -16))
                   (swap! sheight (partial + -16))
                   (zat/set-window-size! terminal {:width @swidth  :height @sheight}))
              \] (do
                   (swap! swidth inc)
                   (swap! sheight inc)
                   (zat/set-window-size! terminal {:width @swidth  :height @sheight}))
              \[ (do
                   (swap! swidth dec)
                   (swap! sheight dec)
                   (zat/set-window-size! terminal {:width @swidth  :height @sheight}))
              \f (zat/set-window-size! terminal (last fullscreen-sizes))
              \w (zat/set-window-size! terminal {:width (size->width @last-size)
                                                 :height (size->height @last-size)})
              \q (zat/destroy! terminal)
              nil)))))))
