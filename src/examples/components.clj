(ns examples.components
  (:require [zaffre.terminal :as zat]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.ui :as zcui]
            [zaffre.components.render :as zcr]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.core.async :refer :all]
            clojure.inspector
            [taoensso.timbre :as log]
            [overtone.at-at :as atat])
  (:import (zaffre.font CompositeFont)))


(def font ztiles/pastiche-16x16) 

(def width 55)
(def height 40)

(def text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.; Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(def updater (zc/empty-state))

(binding [zc/*updater* updater]
(zc/def-component UI
  [this]
  (let [{:keys [fps show-popup text-value text-value-on-change]} (zc/props this)
        popup (if show-popup
                [[zcui/Popup {} [[:text {} ["popup"]]]]]
                [])]
    (log/debug "UI render " fps)
    (zc/csx
      [zcui/InputSelect {:fps fps} [
        [:terminal {} [
          [:group {:id :ui} [
            [:layer {:id :main} [
              [:view {} [
                [:text {} [(str fps)]]]]
              #_[:view {} [
                [zcui/Input {:style {:cursor-fg [244 208 65 255]}
                             :on-change text-value-on-change} []]
                [zcui/Input {:style {:cursor-fg [65 244 208]}
                             :on-change text-value-on-change} []]
                [zcui/Input {:style {:cursor-fg [208 65 244]}
                             :on-change text-value-on-change} []]]]
              #_[:view {:style {:border 1
                              :border-style :single
                              :text-align :right}} [
                  [zcui/Image {:src "/home/santos/Downloads/Food.png" :style {:clip [0 0 16 16]}}]
                  [zcui/Image {:src "/home/santos/Downloads/Food.png" :style {:clip [0 (* 5 16) 16 (* 6 16)]}}]
                  [zcui/Image {:src "/home/santos/Downloads/Food.png" :style {:clip [0 (* 6 16) 16 (* 7 16)]}}]
                [:text {} [
                  [:text {:style {:fg [255 0 0]}} ["he"]]
                  [:text {:style {:fg [255 255 0]}} ["ll"]]
                  [:text {:style {:fg [0 255 0]}} ["o w"]]
                  [:text {:style {:fg [0 255 255]}} ["or"]]
                  [:text {:style {:fg [0 0 255]}} ["ld"]]
                  [:text {:style {:fg [0 0 0] :bg [255 255 255]}} [text-value]]]]]]
              #_[:view {:style {:border 1 :border-style :double}} [
                [:text {:style {:text-align :right}} [text]]]]]]
            [:layer {:id :popup} [
                [zcui/Popup {} [#_[:text {} ["popup"]]
                                 [:view {:style {:width 20
                                                 :height 20
                                                 :max-width 20
                                                 :max-height 20
                                                 :overflow :hidden}} [
                                   #_[zcui/Image {:src "/home/santos/src/zaffre/earthmap.jpg"}] 
                                   #_[zcui/AnimateProps {:style {
                                                         :position-top (fn [t] (int (- (* 5 (Math/sin (/ t 8000))) 8)))
                                                         :position-left (fn [t] (int (- (* 20 (Math/cos (/ t 8000))) 60)))}} [
                                     [:view {:style {:fg nil :bg nil
                                                     :position-type :absolute}} [
                                       [zcui/Image {:src "/home/santos/src/zaffre/earthmap.jpg"}]]]]]
                                     #_[zcui/AnimateProps {:gen (fn [state-chan close-chan]
                                                          (go-loop
                                                            (let [
                                                              a (thread
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-top 4}})
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-top (interpolate-to 4 10 200)}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-top 10}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-top (interpolate-to 10 4 200)}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-top 4}})
                                                                  (<! (timeout 200)))
                                                              b (thread
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-left 4}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-left 4}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-left (interpolate-to 4 10 200)}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-left 10}})
                                                                  (<! (timeout 200))
                                                                  (>! state-chan
                                                                    {:style {
                                                                      :position-left (interpolate-to 10 4 200)}})
                                                                  (<! (timeout 200)) )]
                                                              (<! (merge [a b]))
                                                              (when (<! close-chan)
                                                                (recur)))))} [
                                         [:view {:style {:width 1
                                                         :position-type :relative}} [
                                           [:text {} ["*"]]]]]]
                                     [zcui/AnimateProps2 {:gen (fn [state-chan open-chan]
                                                          (zcui/cycle open-chan
                                                            (zcui/parallel
                                                              (zcui/sequence state-chan
                                                                {:style {
                                                                  :position-top (zcui/interpolate-to 4 16)}}
                                                                200
                                                                {:style {
                                                                  :position-top 16}}
                                                                200
                                                                {:style {
                                                                  :position-top (zcui/interpolate-to 16 4)}}
                                                                200
                                                                {:style {
                                                                  :position-top 4}}
                                                                200)
                                                              (zcui/sequence state-chan
                                                                {:style {
                                                                  :position-left 4}}
                                                                200
                                                                {:style {
                                                                  :position-left (zcui/interpolate-to 4 16)}}
                                                                200
                                                                {:style {
                                                                  :position-left 16}}
                                                                200
                                                                {:style {
                                                                  :position-left (zcui/interpolate-to 16 4)}}
                                                                200))))} [
                                         [:view {:style {:width 1
                                                         :position-type :relative}} [
                                           [:text {} ["*"]]]]]]]]]]]]
]]]]]]))))
(defn -main [& _]
  (zgl/create-terminal
    [{:id :ui
      :layers [:main :popup] ;; With four layers
      :columns width
      :rows height
      :pos [0 0]
      :font (constantly font)}]
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* width 16)  ;; Screen dimentions in pixels
     :screen-height (* height 16)
     :effects []}
    (fn [terminal] ;; Receive the terminal in a callback
      (binding [zc/*updater* updater]
        ;; Save the last key press in an atom
        (let [first-render (atom true)
              last-key (atom nil)
              width (atom 16)
              frames (atom 0)
              fps (atom 0)
              show-popup (atom true)
              text-value (atom "")
              on-change-text-value (fn [e] (reset! text-value (get e :value)))
              last-dom (atom nil)
              key-event-queue (atom [])
              fps-fn (atat/every 1000
                                 #(do
                                   (log/info "frames " @frames)
                                   (reset! fps @frames)
                                   (reset! frames 0))
                                 zc/*pool*)]
          ;; Every 20ms, draw a full frame
          (zat/do-frame terminal 0
            (binding [zc/*updater* updater]
              #_(when (= @frames 2) (System/exit 0))
              (let [key-in (or @last-key \?)
                    [key-events _] (reset-vals! key-event-queue [])
                    ui (zc/csx [UI {:fps @fps
                                    :show-popup @show-popup
                                    :text-value @text-value
                                    :text-value-on-change on-change-text-value}])
                    dom (zcr/render-into-container terminal
                          @last-dom
                          ui)]
                (assert (zc/element? ui))
                (when @first-render
                  (reset! first-render false)
                  ;; Select the first Input
                  (when-let [first-input-element (first (zcui/input-element-seq dom))]
                    (binding [zc/*current-owner* first-input-element]
                      (let [instance (zc/construct-instance first-input-element)]
                        (zc/set-state! instance {:focused true})))))
                (reset! last-dom dom)
                ;; pump all terminal events to elements
                (zce/send-events-to-dom key-events dom)
                (log/trace "--- End of Frame ---")
                (swap! frames inc))))
              
          ;; Wire up terminal events to channels we read from
          (zevents/add-event-listener terminal :keypress
            (fn [new-key]
              ;; add to event queue
              (swap! key-event-queue #(conj % new-key))
              ;; Save last key
              (reset! last-key new-key)
              ;; Make the `q` key quit the application
              (case new-key
                \t (log/set-level! :trace)
                \d (log/set-level! :debug)
                \i (log/set-level! :info)
                \s (clojure.inspector/inspect-tree @last-dom)
                \q (do
                     (atat/stop fps-fn)
                     (atat/stop-and-reset-pool! zc/*pool* :strategy :kill)
                     (zat/destroy! terminal))
                nil))))))))

