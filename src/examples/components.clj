(ns examples.components
  (:require [zaffre.terminal :as zat]
            [zaffre.components :as zc]
            [zaffre.components.events :as zce]
            [zaffre.components.ui :as zcui]
            [zaffre.components.render2 :as zcr]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.color :as zcolor]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            clojure.inspector
            [gossamer.core-graal :as g]
            [taoensso.timbre :as log]
            [overtone.at-at :as atat])
  (:import (zaffre.font CompositeFont)
           (java.time LocalDateTime)
           (java.time.format DateTimeFormatter)))

(def font @ztiles/pastiche-16x16) 

(def width 55)
(def height 40)

(defn lazy-sin [steps]
  (let [step (/ (* 2 3.14159) steps)]
    (log/info "lazy-sin" step)
    (take steps (map (fn [s] (Math/sin s)) (range 0 (* 2 3.14159) step)))))
(defn lazy-cos [steps]
  (let [step (/ (* 2 3.14159) steps)]
    (log/info "lazy-cos" step)
    (take steps (map (fn [s] (Math/cos s)) (range 0 (* 2 3.14159) step)))))

(def text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.; Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")


(defn now [] (LocalDateTime/now))

(zcr/defcomponent Clock
  [props _]
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss.SSS")
        [t set-t!] (g/use-state (.format (now) formatter))]
    (g/use-effect (fn []
      (future
        (loop []
          (let [next-t (.format (now) formatter)]
            #_(log/info "Time" t)
            (set-t! next-t))
          #_(Thread/sleep 1000)
          (recur)))) [])
    [:text {} t]))


(zcr/defcomponent FPSMeter
  [props _]
  (let [reducer (fn [state action]
                  #_(log/info "FPSMeter got state" state "action" action)
                  (case action
                    :frame (update state :frames inc)
                    :update (-> state
                              (assoc :fps (get state :frames))
                              (assoc :frames 0))
                    (assert false)))
        [state dispatch! :as r] (g/use-reducer reducer {:frames 0 :fps 0})]
    (g/use-effect (fn []
      (zcr/request-animation-frame (fn []
        ;(log/info "Animation frame" r state dispatch!)
        (dispatch! :frame))))
      [(get state :frames)])
    (g/use-effect (fn []
      (future
        (loop []
          #_(log/info "Fps tick")
          (dispatch! :update)
          (Thread/sleep 1000)
          (recur)))) [])
    [:view {}
      [:text {:key "fps"} (str "fps: " (get state :fps))]
      [zcui/ProgressBar {:key "fps-bar" :value (/ (get state :fps) 10)}]]))

(zcr/defcomponent UI
  [{:keys [fps show-popup text-value text-value-on-change]} _]
  (let [popup (if show-popup
                [[zcui/Popup {} [[:text {} ["popup"]]]]]
                [])]
    (log/debug "UI render " fps)
    [:terminal {}
      [:group {:id :ui}
        [:layer {:id :main}
          [:view {:key "inputs"}
            [Clock {:key "clock"}]
            [FPSMeter {:key "fpsmeter"}]
            [zcui/OutsetPanel {:key "input-panel" :title "Inputs"
                         :style {:display :flex :flex-direction :row}}
              [zcui/InsetPanel {:key "input-panel" :title "Text" :style {:width 20}}
                [zcui/Input {:key "input1"
                             :autofocus true
                             :style {:cursor-fg (zcolor/color 244 208 65 255)}
                             :on-change text-value-on-change} []]
                [zcui/Input {:key "input2"
                             :style {:cursor-fg (zcolor/color 65 244 208)}
                             :on-change text-value-on-change} []]
                [zcui/Input {:key "input3"
                             :style {:cursor-fg (zcolor/color 208 65 244)}
                             :on-change text-value-on-change} []]]
              [zcui/InsetPanel {:key "radio-panel" :title "Radio Group" :style {:width 20}}
                [zcui/Radio {:key "option1" :name "group1" :value "option1"} [:text {} "Option 1"]]
                [zcui/Radio {:key "option2" :name "group1" :value "option2"} [:text {} "Option 2"]]
                [zcui/Radio {:key "option3" :name "group1" :value "option3"} [:text {} "Option 3"]]]
              [zcui/InsetPanel {:key "checkbox-panel" :title "Checkboxes" :style {:width 20}}
                [zcui/Checkbox {:key "option1" :value "option1"} [:text {} "Option 1"]]
                [zcui/Checkbox {:key "option2" :value "option2"} [:text {} "Option 2"]]
                [zcui/Checkbox {:key "option3" :value "option3"} [:text {} "Option 3"]]]
              [zcui/InsetPanel {:key "button-panel" :title "Buttons"
                           :style {
                             :width 20
                             :display :flex
                             :flex-direction :row}}
                [zcui/Button {:key "option1"} [:text {} "Button1"]]
                [zcui/Button {:key "option2"} [:text {} "Button2"]]
                [zcui/Button {:key "option3"} [:text {} "Button3"]]]]]
          [:view {:key "images"
                  :style {:display :flex
                          :flex-direction :row
                          :border 1
                          :border-style :single
                          :text-align :right}}
              ; From https://opengameart.org/content/64-16x16-food-sprites
              [zcui/Image {:key "img1" :src "https://opengameart.org/sites/default/files/Food.png" :style {:clip [0 0 16 16]}}]
              [zcui/Image {:key "img2" :src "https://opengameart.org/sites/default/files/Food.png" :style {:clip [0 (* 5 16) 16 (* 6 16)]}}]
              [zcui/Image {:key "img3" :src "https://opengameart.org/sites/default/files/Food.png" :style {:clip [0 (* 6 16) 16 (* 7 16)]}}]]
          [:text {:key "hello"}
            [:text {:key 1 :style {:color (zcolor/color 255 0 0)}} "he"]
            [:text {:key 2 :style {:color (zcolor/color 255 255 0)}} "ll"]
            [:text {:key 3 :style {:color (zcolor/color 0 255 0)}} "o w"]
            [:text {:key 4 :style {:color (zcolor/color 0 255 255)}} "or"]
            [:text {:key 5 :style {:color (zcolor/color 0 0 255)}} "ld"]
            [:text {:key 6 :style {:color (zcolor/color 0 0 0) :bg (zcolor/color 255 255 255)}} text-value]]
          [:view {:key "lorem" :style {:border 1 :border-style :double}}
            [:text {:style {:text-align :right}} text]]]
        #_[:layer {:id :popup}
            [zcui/Popup {} [#_[:text {} ["popup"]]
                             [:view {:style {:width 20
                                             :height 20
                                             :max-width 20
                                             :max-height 20
                                             :overflow :hidden}}
                               [zcui/Image {:src "/home/santos/src/zaffre/earthmap.jpg"}] 
                               #_[zcui/AnimateProps {:gen (fn [state-chan open-chan]
                                                           (zutil/cycle open-chan
                                                             (zutil/sequence state-chan
                                                               {:style {
                                                                 :top (fn [steps]
                                                                                  (map (fn [x]
                                                                                         (int (- (* 5 x) 8)))
                                                                                    (lazy-sin steps)))
                                                                 :left (fn [steps]
                                                                                  (map (fn [x]
                                                                                         (int (- (* 20 x) 60)))
                                                                                    (lazy-cos steps)))}}
                                                                  8000)))} [
                                 [:view {:style {:color nil :background-color nil
                                                 :top 0
                                                 :left 0
                                                 :position :absolute}} [
                                   [zcui/Image {:src "/home/santos/src/zaffre/earthmap.jpg"}]]]]]
                                 #_[zcui/AnimateProps {:gen (fn [state-chan open-chan]
                                                             (let [x-min 4
                                                                   y-min 4
                                                                   x-max 16
                                                                   y-max 16]
                                                      (zutil/cycle open-chan
                                                        (zutil/parallel
                                                          (zutil/sequence state-chan
                                                            {:style {
                                                              :top (zcui/interpolate-to y-min y-max)}}
                                                            200
                                                            {:style {
                                                              :top y-max}}
                                                            200
                                                            {:style {
                                                              :top (zcui/interpolate-to y-max y-min)}}
                                                            200
                                                            {:style {
                                                              :top y-min}}
                                                            200)
                                                          (zutil/sequence state-chan
                                                            {:style {
                                                              :left x-min}}
                                                            200
                                                            {:style {
                                                              :left (zcui/interpolate-to x-min x-max)}}
                                                            200
                                                            {:style {
                                                              :left x-max}}
                                                            200
                                                            {:style {
                                                              :left (zcui/interpolate-to x-max x-min)}}
                                                            200)))))} [
                                     [:view {:style {:width 1
                                                     :position :relative}}
                                       [:text {} ["*"]]]]]]
]]]]]))

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
      ;; Save the last key press in an atom
      (let [last-key (atom nil)
            show-popup (atom true)
            last-dom (atom nil)
            key-event-queue (atom [])]
        (zcr/render terminal UI
           {:show-popup @show-popup})

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
              #_#_\s (clojure.inspector/inspect-tree @last-dom)
              \q (do
                   (atat/stop-and-reset-pool! zc/*pool* :strategy :kill)
                   (zat/destroy! terminal))
              nil)))))))

