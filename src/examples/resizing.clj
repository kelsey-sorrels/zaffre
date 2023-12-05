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

(def small-font-fn (constantly (zfont/construct (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true))))
(def medium-font-fn (constantly (zfont/construct (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true))))
(def large-font-fn (constantly (zfont/construct (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 3 true))))
(defn -main [& _]
  ;; render in background thread
  (zgl/create-terminal
    {:app {
       :layers [:text :rainbow]
       :columns 16
       :rows 16
       :pos [0 0]
       :font medium-font-fn}}
    {:title "Zaffre demo"
     :screen-width (* 16 16)
     :screen-height (* 16 16)
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]
     :icon-paths ["images/icon-16x16.png"
                  "images/icon-32x32.png"
                  #_"images/icon-128x128.png"]}
    (fn [terminal]
      (let [term-pub    (zat/pub terminal)
            close-chan  (async/chan)
            fullscreen-sizes (zat/fullscreen-sizes terminal)
            last-key    (atom nil)
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
                              (zat/refresh! terminal)))
                              ;; ~30fps
                            (Thread/sleep 33)
                            (recur))]
    (log/info "Fullscreen sizes" fullscreen-sizes)
    (async/sub term-pub :close close-chan)
    (zevents/add-event-listener terminal :font-change
      (fn [{:keys [character-width character-height] :as ev}]
        (log/info "Got :font-change" character-width character-height ev)
        (zat/set-window-size! terminal {:width (* 16 character-width) :height (* 16 character-height)})))
    ;; get key presses in fg thread
    (zevents/add-event-listener terminal :keypress
      (fn [new-key]
        (reset! last-key new-key)
        (log/info "got key" (or (str @last-key) "nil"))
        ;; change font size on s/m/l keypress
        ;; \w for windowed mode, and \f for fullscreen
        (case new-key
          \s (zat/alter-group-font! terminal :app
               small-font-fn)
          \m (zat/alter-group-font! terminal :app
               medium-font-fn)
          \l (zat/alter-group-font! terminal :app
               large-font-fn)
          \f (zat/set-window-size! terminal (last fullscreen-sizes))
          \w (zat/set-window-size! terminal {:width (* 16 16) :height (* 16 16)})
          \q (zat/destroy! terminal)
          nil)))
    (async/<!! close-chan)
    (System/exit 0)))))
