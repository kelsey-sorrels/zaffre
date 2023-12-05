(ns examples.basic
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil])
  (:import (zaffre.font CompositeFont)))

(def font @ztiles/pastiche-16x16)

(defn -main [& _]
  (zgl/create-terminal
    [{:id :app        ;; Setup a layer group `:app`
      :layers [:text] ;; With one layer `:text`
      :columns 16     ;; 16 characters wide
      :rows 16        ;; 16 characters tall
      :pos [0 0]      ;; With no position offset
      :font (constantly font)}] ;; Give the group a nice font
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* 16 16)  ;; Screen dimentions in pixels
     :screen-height (* 16 16)} ;; Since our font is 16x16 and our layer group
                               ;; is also 16x16
    (fn [terminal]     ;; Receive the terminal in a callback
      ;; Save the last key press in an atom
      (let [last-key (atom nil)
            mods (atom {:shift nil :control nil :alt nil})]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33
          (let [key-in (or @last-key \?)]
            ;; Draw strings
            (zutil/put-string terminal :text 0 0 "Hello world")
            (zutil/put-string terminal :text 0 1 (str key-in))
            (zutil/put-string terminal :text 0 2 (str "shift:" (get @mods :shift)))
            (zutil/put-string terminal :text 0 3 (str "control:" (get @mods :control)))
            (zutil/put-string terminal :text 0 4 (str "alt:" (get @mods :alt)))))
        ;; Wire up terminal events to channels we read from
        (zevents/add-event-listener terminal :keydown
          (fn [new-key]
            (println "keydown" new-key)
            (when (contains? #{:lshift :rshift} new-key)
              (swap! mods merge {:shift true}))
            (when (contains? #{:lcontrol :rcontrol} new-key)
              (swap! mods merge {:control true}))
            (when (contains? #{:lalt :ralt} new-key)
              (swap! mods merge {:alt true}))))
        (zevents/add-event-listener terminal :keyup
          (fn [new-key]
          (println "keyup" new-key)
            (when (contains? #{:lshift :rshift} new-key)
              (swap! mods merge {:shift false}))
            (when (contains? #{:lcontrol :rcontrol} new-key)
              (swap! mods merge {:control false}))
            (when (contains? #{:lalt :ralt} new-key)
              (swap! mods merge {:alt false}))))
        (zevents/add-event-listener terminal :keypress
          (fn [new-key]
            (println "keypress" new-key)
            ;; Save last key
            (reset! last-key new-key)
            ;; Make the `q` key quit the application
            (case new-key
              \q (zat/destroy! terminal)
              nil)))))))


