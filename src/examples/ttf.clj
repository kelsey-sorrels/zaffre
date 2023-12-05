(ns examples.ttf
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [<! <!! go-loop]])
  (:import (zaffre.font TTFFont)))

(def font (TTFFont. "Ubuntu Mono" 16 true))
(defn -main [& _]
  (zgl/create-terminal
    {:app {           ;; Setup a layer group `:app`
      :layers [:text] ;; With one layer `:text`
      :columns 16     ;; 16 characters wide
      :rows 16        ;; 16 characters tall
      :pos [0 0]      ;; With no position offset
      :font (constantly font)}} ;; Give the group a nice font
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* 16 16)  ;; Screen dimentions in pixels
     :screen-height (* 16 16)} ;; Since our font is 16x16 and our layer group
                               ;; is also 16x16
    (fn [terminal]     ;; Receive the terminal in a callback
      ;; Save the last key press in an atom
      (let [last-key (atom nil)]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33
          (let [key-in (or @last-key \?)]
            ;; Draw strings
            (zutil/put-string terminal :text 0 0 "Hello world")
            (zutil/put-string terminal :text 12 0 (str key-in))))
        ;; Wire up terminal events to channels we read from
        (zevents/add-event-listener terminal :keypress
          (fn [new-key]
            ;; Save last key
            (reset! last-key new-key)
            ;; Make the `q` key quit the application
            (case new-key
              \q (zat/destroy! terminal)
              nil)))))))


