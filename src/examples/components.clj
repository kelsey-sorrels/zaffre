(ns examples.components
  (:require [zaffre.terminal :as zat]
            [zaffre.components :as zc]
            [zaffre.components.render :as zcr]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.core.async :refer :all]
            [overtone.at-at :as atat])
  (:import (zaffre.font CompositeFont)))

(defmethod zc/render-comp :ui [_ props]
  [:terminal {}
    [:group {:group-id :ui}
      (zc/with-children :layer {:layer-id :main}
        (get props :children))]])

(def font ztiles/pastiche-16x16)

(def width 12)
(def height 2)
(defn -main [& _]
  (zgl/create-terminal
    [{:id :ui
      :layers [:main] ;; With four layers
      :columns width
      :rows height
      :pos [0 0]
      :font (constantly font)}]
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* width 16)  ;; Screen dimentions in pixels
     :screen-height (* height 16)
     :effects []}
    (fn [terminal] ;; Receive the terminal in a callback
      ;; Save the last key press in an atom
      (let [last-key (atom nil)
            render-state (atom {})]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33 
          (let [key-in (or @last-key \?)]
            ;; Draw components
            (zcr/render-into-container terminal render-state
              [:ui {}
                [:label {:x 5 :y 2}  "hello world"]])))
        ;; Wire up terminal events to channels we read from
        (zevents/add-event-listener terminal :keypress
           (fn [new-key]
            ;; Save last key
            (reset! last-key new-key)
            ;; Make the `q` key quit the application
            (case new-key
              \q (zat/destroy! terminal)
              nil)))))))
