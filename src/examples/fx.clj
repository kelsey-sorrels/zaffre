(ns examples.fx
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

(def font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 false))

(defn -main [& _]
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
     :default-bg-color [5 5 8]}
    (fn [terminal]
      (let [last-key    (atom nil)]
        ;; Every 10ms, set the "Rainbow" text to have a random fg color
        (go-loop []
          (dosync
            (doseq [x (range (count "Rainbow"))
                    :let [rgb (hsv->rgb (double (rand 360)) 1.0 1.0)]]
                (zat/set-fx-fg! terminal :rainbow (inc x) 1 rgb)))
            (zat/refresh! terminal)
          (Thread/sleep 10)
          (recur))
            ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33
          (let [key-in (or @last-key \?)]
            (zutil/put-string terminal :text 0 0 "Hello world")
            (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
              (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
            (zutil/put-string terminal :text 12 0 (str key-in))
            (zutil/put-string terminal :rainbow 1 1 "Rainbow")))
      ;; get key presses in fg thread
      (zevents/add-event-listener terminal :keypress
        (fn [new-key]
          (reset! last-key new-key)
          (case new-key
            ;; quit on \q keypress
            \q (zat/destroy! terminal)
            nil)))))))


