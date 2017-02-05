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

(def font ztiles/pastiche-16x16)

(def background [
  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
  "░┌─┬                 ░░                 ┬─┐░"
  "░├─░                 ░░                 ░─┤░"
  "░├─░═════════════════░░═════════════════░─┤░"
  "░├─░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░─┤░"
  "░├─░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░─┤░"
  "░├─░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░─┤░"
  "░├─░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░─┤░"
  "░├─░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░─┤░"
  "░├*░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░░  ▀ ▀ ▀ ▀ ▀ ▀ ▀  ░*┤░"
  "░├▲░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░▲┤░"
  "░└──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘░"
  "░──────────────────────────────────────────░"
  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"
  "░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░"])

(def bg-chars (for [[y line] (map-indexed vector background)
                    [x c]    (map-indexed vector line)]
                {:c (char c) :x x :y y :fg [255 255 255] :bg [0 0 0]
                 :style (if (or (and (< 0 x 3) (< 0 y 12))
                                (and (< 40 x 43) (< 0 y 12))
                                (and (< 0 x 43) (< 10 y 12)))
                          #{:fg-bg}
                          #{})}))
(println (vec bg-chars))
(def pink [165   0 170])
(def green [  0 161  30])
(def yellow [255 255 0])
(def black [0 0 0])
(def sign-chars [
  {:c \* :x 2  :y 9  :fg yellow :bg black}
  {:c \* :x 41 :y 9  :fg yellow :bg black}
  {:c \S :x 20 :y 4  :fg pink   :bg black}
  {:c \H :x 20 :y 5  :fg pink   :bg black}
  {:c \O :x 20 :y 6  :fg pink   :bg black}
  {:c \P :x 20 :y 7  :fg pink   :bg black}
  {:c \B :x 23 :y 6  :fg green  :bg black}
  {:c \A :x 23 :y 7  :fg green  :bg black}
  {:c \R :x 23 :y 8  :fg green  :bg black}
  {:c \@ :x 23 :y 14 :fg green  :bg black}])

(def width (count (first background)))
(def height (count background))
(defn -main [& _]
  (zaw/create-animated-terminal
    zgl/create-terminal
    [{:id :map
      :layers [:background :fx] ;; With four layers
      :columns width
      :rows height
      :pos [0 0]
      :font (constantly font)}
     {:id :lighting
      :layers [:lighting] ;; With four layers
      :columns width
      :rows height
      :pos [0 0]
      :font (constantly font)
      :gl-blend-equation :gl-func-add
      :gl-blend-func [:gl-dst-color :gl-zero]}
     {:id :signs
      :layers [:signs] ;; With four layers
      :columns width
      :rows height
      :pos [0 0]
      :font (constantly font)}]
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* width 8)  ;; Screen dimentions in pixels
     :screen-height (* height 8)
     :effects [
       (zaw/make-lighting-effect :lighting width height [
         {:x 16 :y 8  :z 50  :intensity 400 :color [ 25  25 255]}
         {:x 3  :y 9  :z 2.5 :intensity 8.5 :color [181 131   0]}
         {:x 41 :y 9  :z 2.5 :intensity 8.5 :color [181 131   0]}
         {:x 20 :y 11 :z 3   :intensity 8   :color [165   0 170]}
         {:x 23 :y 11 :z 3   :intensity 9   :color [  0 161  30]}])
       (zaw/make-rain-effect :fx width height)]} ;; Since our font is 16x16 and our layer group
                               ;; is also 16x16
    (fn [terminal] ;; Receive the terminal in a callback
      ;; Save the last key press in an atom
      (let [last-key (atom nil)]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33 []
          (let [key-in (or @last-key \?)]
            ;; Draw strings
            (zat/put-chars! terminal :background bg-chars)
            (zat/put-chars! terminal :signs sign-chars)))
        ;; Wire up terminal events to channels we read from
        (zevents/add-event-listener terminal :keypress
           (fn [new-key]
            ;; Save last key
            (reset! last-key new-key)
            ;; Make the `q` key quit the application
            (case new-key
              \q (zat/destroy! terminal)
              nil)))))))

