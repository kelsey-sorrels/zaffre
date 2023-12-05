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

(defn dot [u v]
  (reduce + 0 (map * u v)))

(defn norm [v]
  (Math/sqrt (reduce + 0 (map (fn [x] (* x x)) v))))
  
(defn unit [v]
  (let [l (norm v)]
    (map #(/ % l) v)))

(def font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 false))

(defn -main [& _]
  (zgl/create-terminal
    ;; base layer is text
    {:app {
       :layers [:text]
       :columns 16
       :rows 16
       :pos [0 0]
       :font (constantly font)}
     ;; fx-mul layer is a multiplicative blend layer
     :fx-mul {
       :layers [:dark]
       :columns 16
       :rows 16
       :pos [0 0]
       :font (constantly font)
       :gl-blend-equation :gl-func-add
       :gl-blend-func [:gl-dst-color :gl-zero]}
     :fx-add {
       :layers [:light]
       :columns 16
       :rows 16
       :pos [0 0]
       :font (constantly font)
       :gl-blend-equation :gl-func-add
       :gl-blend-func [:gl-one :gl-one]}}
    {:title "Zaffre demo"
     :screen-width (* 16 16)
     :screen-height (* 16 16)
     :default-fg-color [250 250 250]
     :default-bg-color [5 5 8]}
    (fn [terminal]
      (let [last-key    (atom nil)]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33
          (let [key-in (or @last-key \?)
                t      (/ (.getTime (new java.util.Date)) 1000)
                ;; light pos
                lx     (+ (* 5 (Math/cos t)) 8.0)
                ly     (+ (* 5 (Math/sin t)) 8.0)]
            ; text
            (doseq [y (range 16)]
              (zutil/put-string terminal :text 0 y "a b c d e f g h "))
            (zutil/put-string terminal :text 0 0 "Hello world")
            (zutil/put-string terminal :text 12 0 (str key-in))
            ; fx (note the only characters we're drawing if full-cell boxes
            (doseq [y (range 16)]
              (zutil/put-string terminal :dark 0 y (apply str (repeat 16 " ")))
              (zutil/put-string terminal :light 0 y (apply str (repeat 16 " "))))
            (zat/set-bg! terminal :light 8 8 [32 32 16])
            (doseq [x (range 16)
                    y (range 16)]
                (zat/set-bg! terminal :dark x y [32, 32, 128])
                (let [rlx (- lx x) ;; vector from x, y to lx, ly
                      rly (- ly y)
                      rlz 3
                      rl  [rlx rly rlz]
                      d   (norm rl) ; distance from [x y] to light
                      i (min 1.0 (/ (* 8.5 (dot (unit [0 0 1]) (unit rl))) (* d d)))]
                  (zat/set-bg! terminal :light x y (mapv (partial * i) [255 255 128]))))))
        ;; get key presses in fg thread
        (zevents/add-event-listener terminal :keypress
          (fn [new-key]
            (reset! last-key new-key)
            (case new-key
              ;; quit on \q keypress
              \q (zat/destroy! terminal)
              nil)))))))


