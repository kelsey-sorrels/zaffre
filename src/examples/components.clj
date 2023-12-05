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
        (get props :zaffre/children))]])

(def font ztiles/pastiche-16x16)

(def width 30)
(def height 40)

(def text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.; Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(zc/def-component UI
  [this]
  (zc/csx [:terminal {} [
    (zc/csx [:group {:id :ui} [
      (zc/csx [:layer {:id :main} [
        (zc/csx [:view {} [
          (zc/csx [:text {} [text]])]])]])]])]]))

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
    (binding [zc/*updater* (zc/empty-state)]
      (fn [terminal] ;; Receive the terminal in a callback
        ;; Save the last key press in an atom
        (let [last-key (atom nil)
              width (atom 16)
              render-state (atom {})]
          ;; Every 33ms, draw a full frame
          (zat/do-frame terminal 33 
            (let [key-in (or @last-key \?)]
              ;; Draw components
              (zcr/render-into-container terminal
                (zc/csx [UI])
                #_[:ui {}
                  [:text {} (str @width)]
                  [:view {:zaffre/style {:left 1 :top 1 :width @width}}
                    [:text {}
                      [:text {} text]
                      #_[:text {:fg [255 0 0 255]} "llo"]
                      #_[:text {:fg [255 128 0 255]} "wor"]
                      #_[:text {:fg [255 255 0 255]} "ld"]]]])))
          ;; Wire up terminal events to channels we read from
          (zevents/add-event-listener terminal :keypress
             (fn [new-key]
              ;; Save last key
              (reset! last-key new-key)
              ;; Make the `q` key quit the application
              (case new-key
                \b (swap! width inc)
                \s (swap! width dec)
                \q (zat/destroy! terminal)
                nil))))))))

