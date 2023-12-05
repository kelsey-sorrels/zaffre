(ns examples.resizing
  (:require [zaffre.aterminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.aterminal ATerminal)
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

(defn -main [& _]
  ;; render in background thread
  (zgl/make-terminal
    [:text :rainbow]
    {:title "Zaffre demo"
     :columns 80 :rows 24
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]
     :windows-font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true)
     :else-font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true)
     ;; Alternatively
     ;:windows-font (TTFFont. "Consolas" 12)
     ;:else-font (TTFFont. "Monospaced" 12)
     :icon-paths ["images/icon-16x16.png"
                  "images/icon-32x32.png"
                  "images/icon-128x128.png"]}
    (fn [terminal]
      (let [term-pub    (zat/pub terminal)
            key-chan    (async/chan)
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
    (async/sub term-pub :keypress key-chan)
    (async/sub term-pub :close close-chan)
    ;; get key presses in fg thread
    (go-loop []
      (let [new-key (async/<! key-chan)]
        (reset! last-key new-key)
        (log/info "got key" (or (str @last-key) "nil"))
        ;; change font size on s/m/l keypress
        ;; \w for windowed mode, and \f for fullscreen
        (case new-key
          \s (zat/apply-font! terminal
               (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true)
               (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1 true))
          \m (zat/apply-font! terminal
               (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true)
               (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true))
          \l (zat/apply-font! terminal
               (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 3 true)
               (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 3 true))
          \f (zat/fullscreen! terminal (first fullscreen-sizes))
          \w (zat/fullscreen! terminal false)
          \q (zat/destroy! terminal)
          nil)))
    (let [_ (async/<! close-chan)]
      (async/close! fx-chan)
      (async/close! render-chan)
      (System/exit 0))))))
