(ns examples.shaderfx
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
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
(def font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true))
(defn -main [& _]
  ;; render in background thread
   (let [colorShift    (atom 0.0001)
         brightness    (atom 0.68)
         contrast      (atom 2.46)
         scanlineDepth (atom 0.94)
         time          (atom 0.0)
         noise         (atom 0.0016)]
     (zgl/create-terminal
       {:app {
          :layers [:text :rainbow]
          :columns 16
          :rows 16
          :pos [0 0]
          :font (constantly font)}}
       {:title "Zaffre demo"
        :screen-width (* 16 16)
        :screen-height (* 16 16)
        :default-fg-color [250 250 250]
        :default-bg-color [5 5 8]
        :fx-shader {:name     "retro.fs"
                    :uniforms [["time" @time]
                               ["noise" @noise]
                               ["colorShift" @colorShift]
                               ["scanlineDepth" @scanlineDepth]
                               ["brightness" @brightness]
                               ["contrast" @contrast]]}}
       (fn [terminal]
         (let [last-key    (atom nil)]
           (zat/do-frame terminal 33
             (let [key-in (or @last-key \?)]
               (zutil/put-string terminal :text 0 0 "Hello world")
               (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                 (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
               (zutil/put-string terminal :text 12 0 (str key-in))
               (zutil/put-string terminal :rainbow 1 1 "Rainbow")))
           (go-loop []
             (dosync
               (doseq [x (range (count "Rainbow"))
                       :let [rgb (hsv->rgb (double (rand 360)) 1.0 1.0)]]
                   (zat/set-fx-fg! terminal :rainbow (inc x) 1 rgb)))
               (zat/assoc-shader-param! terminal "time" (swap! time inc))
               (zat/refresh! terminal)
             (Thread/sleep 10)
             (recur))
           ;; get key presses in callback
           (zevents/add-event-listener terminal :keypress
             (fn [new-key]
               (reset! last-key new-key)
               (log/info "got key" (or (str @last-key) "nil"))
               ;; change font size on s/m/l keypress
               (case new-key
                 \0 (zat/assoc-shader-param! terminal "brightness" (swap! brightness #(- % 0.02)))
                 \1 (zat/assoc-shader-param! terminal "brightness" (swap! brightness #(+ % 0.02)))
                 \2 (zat/assoc-shader-param! terminal "contrast" (swap! contrast #(- % 0.02)))
                 \3 (zat/assoc-shader-param! terminal "contrast" (swap! contrast #(+ % 0.02)))
                 \4 (zat/assoc-shader-param! terminal "scanlineDepth" (swap! scanlineDepth #(- % 0.02)))
                 \5 (zat/assoc-shader-param! terminal "scanlineDepth" (swap! scanlineDepth #(+ % 0.02)))
                 \6 (zat/assoc-shader-param! terminal "colorShift" (swap! colorShift #(- % 0.0001)))
                 \7 (zat/assoc-shader-param! terminal "colorShift" (swap! colorShift #(+ % 0.0001)))
                 \8 (zat/assoc-shader-param! terminal "noise" (swap! noise #(- % 0.005)))
                 \9 (zat/assoc-shader-param! terminal "noise" (swap! noise #(+ % 0.005)))
                 \p (log/info "brightness" @brightness
                              "contrast" @contrast
                              "scanlineDepth" @scanlineDepth
                              "colorShift" @colorShift
                              "noise" @noise
                              "time" @time)
                 \q (zat/destroy! terminal)
                 nil))))))))


