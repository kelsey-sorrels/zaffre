(ns examples.animation
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.animation.wrapper :as zaw]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.core.async :refer :all]
            [overtone.at-at :as atat])
  (:import (zaffre.font CompositeFont)))

(def font (CompositeFont. [ztiles/pastiche-16x16
                           ztiles/fantasy]))
(defn -main [& _]
  (zaw/create-animated-terminal
    zgl/create-terminal
    {:app {           ;; Setup a layer group `:app`
      :layers [:text :fx] ;; With two layers `:text` and `:fx`
      :columns 16     ;; 16 characters wide
      :rows 16        ;; 16 characters tall
      :pos [0 0]      ;; With no position offset
      :font (constantly font)}} ;; Give the group a nice font
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* 16 16)  ;; Screen dimentions in pixels
     :screen-height (* 16 16)
     :filters []
     :effects [(zaw/make-rain-effect :fx 16 16)]} ;; Since our font is 16x16 and our layer group
                               ;; is also 16x16
    (fn [terminal] ;; Receive the terminal in a callback
      ;; Save the last key press in an atom
      (let [last-key (atom nil)]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33 [:text]
          (let [key-in (or @last-key \?)]
            ;; Draw strings
            (zat/put-chars! terminal :text (zutil/mk-string 0 0 "Hello world"))
            (zat/put-chars! terminal :text (zutil/mk-string 12 0 (str key-in)))))
        ;; Wire up terminal events to channels we read from
        (zevents/add-event-listener terminal :keypress
           (fn [new-key]
            ;; Save last key
            (reset! last-key new-key)
            ;; Make the `q` key quit the application
            (case new-key
              \q (zat/destroy! terminal)
              nil)))))))

