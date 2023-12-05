(ns zaffre.components.ui
  (:require
    [overtone.at-at :as atat]
    [gossamer.core-graal :as g]
    [clj-http.client :as client]
    [clojure.java.io :as jio]
    [clojure.core.cache :as cache]
    [clojure.core.async :refer [go-loop timeout <!]]
    [clojure.pprint :as pprint]
    [taoensso.timbre :as log]
    rockpick.core
    [zaffre.color :as zcolor]
    [zaffre.components :as zc]
    [zaffre.font :as zfont]
    [zaffre.imageutil :as ziu]))

(log/set-level! :info)

; From merge-with Clojure cheatsheet
(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
    only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
maps))

(def resource-cache (atom (cache/ttl-cache-factory {} :ttl 30000)))
(def image-cache (atom (cache/fifo-cache-factory {} :ttl 30000)))

(defn to-input-char [event]
  (if (= event :space)
    \
    event))

(g/defcomponent Input
  [props _]
  (let [[value set-value!] (g/use-state (get props :value ""))
        [focused set-focus!] (g/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (g/use-state false)
        max-length (or (get props :max-length) 28)
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [e]
          (log/info "Input on-keypress" e)
          (let [k (get e :key)]
            (log/info "value" value "max-length" max-length "k" k)
            (cond
              (= k :backspace)
                (set-value! (subs value 0 (dec (count value))))
              (and (or (char? k) (= k :space)) (not= k \newline))
                (set-value! (if (< (count value) max-length)
                              (str value (to-input-char k))
                              value)))))
        default-props {:max-length max-length
                       :on-focus on-focus
                       :on-blur on-blur
                       :on-keypress on-keypress
                       :gossamer.components/focusable true
                       :style {:width 30
                               :height 1
                               :display :flex
                               :flex-direction :row
                               :cursor-char-on \u2592
                               :cursor-char-off \_
                               :cursor-fg :ref/secondary
                               :cursor-bg :ref/surface}}
        props (assoc (merge default-props props)
                :style (merge (:style default-props)
                              (:style props)))
        {:keys [width
                cursor-char-on
                cursor-char-off
                cursor-fg
                cursor-bg]} (get props :style)
        cursor-fg (if (and focused show-cursor) cursor-fg :ref/on-surface)
        cursor (if (and focused show-cursor) cursor-char-on cursor-char-off)]

     (g/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

     #_(log/info "Input value" value (type value))
     #_(log/info "props" props)
     #_(log/debug "Input render" show-cursor (dissoc props :children))
     #_(log/info "Input width" width (type width))
     #_(log/info "cursor fg" cursor-fg "cursor-bg" cursor-bg "cursor" cursor)
     [:view props
       [:text {:key "text"} value]
       [:text {:key "cursor" :style {:color cursor-fg :background-color cursor-bg}} (str cursor)]
       [:text {:key "rest"} (apply str (repeat (- width (count value)) "_"))]]))

(g/defcomponent Button
  [props _]
  (let [[focused set-focus!] (g/use-state (get props :focus false))
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [_] nil)
        {:keys [name children]} props
        color (if focused :ref/on-secondary :ref/on-surface)
        background-color (if focused :ref/secondary :ref/surface)
        overlay-percent (if focused 0 12)
        element-style {:color color
                       :background-color background-color
                       :overlay-percent overlay-percent}
        default-props {:on-focus on-focus
                       :on-blur on-blur
                       :on-keypress on-keypress
                       :gossamer.components/type :button
                       :gossamer.components/focusable true
                       :style {:display :flex
                               :flex-direction :row}}
        props (assoc (merge default-props props)
                :style (merge (:style default-props)
                              (:style props)))]

    [:view props
        [:text {:key "button-left" :style (merge element-style {:content :ref/button-left})} ""]
        [:view {:key "button-content" :style element-style} children]
        [:text {:key "button-right" :style (merge element-style {:content :ref/button-right})} ""]]))

(g/defcomponent Checkbox
  [props _]
  (let [[selected set-selected!] (g/use-state (get props :checked false))
        [focused set-focus!] (g/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (g/use-state false)
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [e]
          (log/info "Checkbox on-keypress" e)
          (set-selected! not))
        {:keys [name value children]} props
        default-props {:on-focus on-focus
                       :on-blur on-blur
                       :on-keypress on-keypress
                       :gossamer.components/type :checkbox
                       :gossamer.components/focusable true
                       :gossamer.components/set-value set-selected!
                       :style {:display :flex
                               :flex-direction :row
                               :cursor-char-on \u2592
                               :cursor-char-off \_
                               :cursor-fg (zcolor/color 255 255 255 255)
                               :cursor-bg (zcolor/color 0 0 0 255)}}
        props (assoc (merge default-props props)
                :style (merge (:style default-props)
                              (:style props)))
        {:keys [cursor-char-on
                cursor-char-off
                cursor-fg
                cursor-bg]} (get props :style)
        cursor (if (and focused show-cursor) (str cursor-char-on) (if selected :ref/checkbox-checked :ref/checkbox-unchecked))
        color (if selected :ref/secondary :ref/on-surface)]
     (g/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

    [:view props
      (concat
        [[:text {:key "checkbox-left" :style {:content :ref/checkbox-left
                                              :color :ref/on-surface}} ""]
         [:text {:key "checkbox-check" :style {:content cursor
                                               :color color}} ""]
         [:text {:key "checkbox-right" :style {:content :ref/checkbox-right
                                           :color :ref/on-surface}} ""]]
        children)]))

(g/defcomponent LoadingSpinner
  [props _]
  (let [{:keys [speed characters] :or {speed 100 characters ["/" "-" "\\" "|"]}} props
        [index set-index!] (g/use-state 0)]
    (g/use-effect (fn []
      (go-loop []
        (<! (timeout speed))
        (set-index! (fn [index] 
          (mod (inc index) (count characters))))
        (recur))) [])
    [:text {:style {:color :ref/on-surface}} (nth characters index)]))

(g/defcomponent Radio
  [props _]
  (let [[selected set-selected!] (g/use-state (get props :checked false))
        [focused set-focus!] (g/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (g/use-state false)
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [e]
          (log/info "Radio on-keypress" e)
          (set-selected! true))
        {:keys [name value children]} props
        default-props {:on-focus on-focus
                       :on-blur on-blur
                       :on-keypress on-keypress
                       :gossamer.components/type :radio
                       :gossamer.components/focusable true
                       :gossamer.components/set-value set-selected!
                       :style {:display :flex
                               :flex-direction :row
                               :cursor-char-on \u2592
                               :cursor-char-off \_
                               :cursor-fg (zcolor/color 255 255 255 255)
                               :cursor-bg (zcolor/color 0 0 0 255)}}
        props (assoc (merge default-props props)
                :style (merge (:style default-props)
                              (:style props)))
        {:keys [cursor-char-on
                cursor-char-off
                cursor-fg
                cursor-bg]} (get props :style)
        cursor (if (and focused show-cursor) (str cursor-char-on) (if selected :ref/radio-checked :ref/radio-unchecked))
        color (if selected :ref/secondary :ref/on-surface)]
     (g/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

    [:view props
      (concat
        [[:text {:key "radio-left" :style {:content :ref/radio-left
                                           :color :ref/on-surface}} ""]
         [:text {:key "radio-check" :style {:content cursor
                                           :color color}} ""]
         [:text {:key "radio-right" :style {:content :ref/radio-right
                                           :color :ref/on-surface}} ""]]
        children)]))

(g/defcomponent ProgressBar
  [props _]
  (let [left (int (get props :value 0))
        right (- 100 left)]
    ; FIXME copy key (props?) into view
    [:view {:key "progress-bar"
            :style {:display :flex
                    :flex-direction :row}}
      [:view {:key "left"
              :style {:background-char :ref/progress-filled
                      :background-color :ref/secondary
                      :width (str left "%")
                      :height 1}}]
      [:view {:key "right"
              :style {:background-char :ref/progess-empty
                      :color :ref/secondary
                      :width (str right "%")
                      :height 1}}]]))


#_(g/defcomponent VScrollBar
  [props _]
  (let [{:keys [height num-items pos]} props]
    [:view {:style {:display :flex
                            :flex-direction :column
                            :width 1
                            :justify-content :space-between
                            :background-color (rcolor/color->rgb :dark-gray)
                            :height "100%"}}
      [:view {}
        [:text {:style {:color (rcolor/color->rgb :highlight)}} ruicommon/up-arrow-char]]
      [:view {:style {:height (- height 2)
                      :background-color (rcolor/color->rgb :darker-gray)}}
        [:text {:style {:color (rcolor/color->rgb :dark-gray)
                        :top (int (/ (* pos height) (dec num-items)))}} ruicommon/cursor-char]]
      [:view {}
        [:text {:style {:color (rcolor/color->rgb :highlight)}} ruicommon/down-arrow-char]]]))

(g/defcomponent ScrollPane
  [props _]
  (let [left (int (get props :value 0))
        right (- 100 left)]
    ; FIXME copy key (props?) into view
    [:view {:key "progress-bar"
            :style {:display :flex
                    :flex-direction :row}}
      [:view {:key "left"
              :style {:background-char \ 
                      :background-color (zcolor/color 255 255 255)
                      :width (str left "%")
                      :height 1}}]
      [:view {:key "right"
              :style {:background-char \u2592
                      :width (str right "%")
                      :height 1}}]]))

(defn async-load-file
  [path on-load on-error]
  (future
    (try
      (let [bytes (get (swap! resource-cache
                              cache/through-cache
                              path
                              (constantly (ziu/slurp-bytes (jio/as-file path))))
                       path)]
        (on-load bytes))
      (catch Throwable t
        (on-error t)))))

(defn async-load-url
  [url on-load on-error]
  (future
    (try
      (let [response (get (swap! resource-cache
                              cache/through-cache
                              url
                              (constantly (client/get url {:as :byte-array})))
                       url)]
        (log/info "async-load-url" response (type response))
        (on-load (:body response)))
      (catch Throwable t
        (on-error t)))))

; Adapted from https://dev.to/gethackteam/from-higher-order-components-hoc-to-react-hooks-2bm9
(defn use-resource
  [src]
  (let [[data set-data!] (g/use-state nil)
        [error set-error!] (g/use-state nil)
        [is-loading set-is-loading!] (g/use-state true)]
    (g/use-effect (fn effect-fn []
        (try
          (log/info "Using resource" src "data" data "error" error "is-loading" is-loading)
          ((if (clojure.string/starts-with? src "http")
             async-load-url
             async-load-file)
           src
           (fn on-load [bytes]
             (log/info "setting data" bytes)
             #_(assert false)
             (set-is-loading! false)
             (set-data! bytes)
             (log/info "finished setting data"))
           (fn on-error [e]
             (log/error "Error loading " src)
             (println e)
             (log/error e)
             #_(assert false)
             (set-is-loading! false)
             (set-error! e)
             (log/info "finished setting error")))
           (log/info "finished using resource")
           (catch Throwable t
             (log/error t))))
     [])
    [data error is-loading]))

(g/defcomponent NativeImage
  [props _]
  (let [{:keys [src data style]} props
        clip (get style :clip)
        [width height img-characters :as memo] (g/use-memo
          (fn []
            (let [img (ziu/load-image data)
                  clipped-img (if clip
                                (ziu/clip-image img clip)
                                img)
                  pixels (ziu/pixel-seq clipped-img)
                  width (ziu/width clipped-img)
                  height (ziu/height clipped-img)
                  lines (partition width pixels)]
              ; TODO: close?
              ;(.close img)
              ;(.close clipped-img)
              (log/debug "NativeImage render" (get style :clip) width "x" height)
              (log/debug "NativeImage render" (count lines))
              #_(doseq [line lines]
                (log/info (vec line)))
              (let [img-characters (mapv (fn [[line1 line2]]
                                           (mapv (fn [px1 px2]
                                                  {:c \u2580 ; ▀
                                                   :fg (zcolor/color
                                                         (conj (mapv (comp (partial bit-and 0xFF) byte) px1) 255))
                                                   :bg (zcolor/color
                                                         (conj (mapv (comp (partial bit-and 0xFF) byte) px2) 255))})
                                                 line1 line2))
                                         (partition 2 lines))]
                (log/trace "img-characters" img-characters)
                (log/info "Loaded image " width "x" height)
                [width height img-characters])))
          [src clip])]
     #_(log/info "memo" memo "width" width "height" height "img-characters" (count img-characters))
     [:img {:width width
            :height (/ height 2)
            :pixels img-characters}]))

(g/defcomponent RexPaintImage
  [props _]
  (let [{:keys [data layer-index]} props
        layer-index (or layer-index 0)
        layers (if (sequential? data) data (rockpick.core/read-xp (clojure.java.io/input-stream data)))
        layer (nth layers layer-index)
        pixels (mapv (fn [line]
                 (mapv (fn [{:keys [ch fg bg]}]
                        (let [fg-r (get fg :r)
                              fg-g (get fg :g)
                              fg-b (get fg :b)
                              fg-a 255
                              bg-r (get bg :r)
                              bg-g (get bg :g)
                              bg-b (get bg :b)
                              bg-a 255]
                        {:c (get zfont/cp437-unicode (int ch))
                         :fg (zcolor/color [fg-r fg-g fg-b fg-a])
                         :bg (zcolor/color [bg-r bg-g bg-b bg-a])}))
                       line))
             layer)]
    (log/trace "pixels" pixels)
    [:img {:width (count (first pixels)) :height (count pixels)} pixels]))

(defn data-to-pixels [data]
  (mapv (fn [[line1 line2]]
          (mapv (fn [px1 px2]
                 {:c \u2580
                  :fg (zcolor/color (conj (mapv (partial bit-and 0xFF) px1) 255))
                  :bg (zcolor/color (conj (mapv (partial bit-and 0xFF) px2) 255))}) ; ▀
               line1 line2))
        (partition 2 data)))

(def ListImage (zc/create-react-class {
  :display-name "ListImage"
  :render (fn [this]
    (let [{:keys [data]} (zc/props this)
          width (reduce max (map count data))
          height (count data)
          pixels (data-to-pixels data)]
      (log/trace "img-characters" pixels)
      (zc/csx [:img {:width width :height (/ height 2)} pixels])))}))

(g/defcomponent DataImage
  [props _]
  (let [{:keys [src data style]} props]
    (if (clojure.string/ends-with? src "xp")
      [RexPaintImage {:src src :data data :style style}]
      [NativeImage {:src src :data data :style style}])))
  
(g/defcomponent Image
  [props _]
  ;; passes :src :style, etc through
  (let [[data error is-loading] (use-resource (get props :src))]
       #_(log/info "resource:" data error is-loading)
    (try
      (cond
        is-loading
          [:text {} "loading"]
        data
          [DataImage (merge props {:data data})]
        error
          [:text {} (str error)]
        :else
          [:text {} (str "Error loading " (get props :src))])
      (catch Throwable t
        (log/error "Error rendering Image")
        (log/error t)))))

(g/defcomponent Panel
  [props _]
  (let [{:keys [title border children]} props]
    (log/info "Panel props" props)
    (log/info "Panel children" children)
    [:view {:style {:border (or border 1)
                    :background-color :ref/surface}}
      (cons [:text {:key "panel-title"
                    :style {:position :absolute
                            :height 1 :width (+ 2 (count title))
                            :top -1 :left 1}}
             (str "\u2524" title "\u251C")]
      children)]))

(g/defcomponent InsetPanel
  [props _]
  (let [{:keys [title border children]} props]
    (log/info "Panel props" props)
    (log/info "Panel children" children)
    [:view {:style {:border (or border 1)
                    :border-color-top :ref/background-overlay-4
                    :border-color-left :ref/background-overlay-4
                    :border-color-bottom :ref/surface-overlay-4
                    :border-color-right :ref/surface-overlay-4
                    :background-color :ref/surface}}
      (cons [:view {:key "panel-title"
                    :style {:position :absolute
                            :height 1 :width (+ 2 (count title))
                            :top -1 :left 1
                            :display :flex
                            :flex-direction :row}}
              [:text {:style {:color :ref/background-overlay-4}} "\u2524"]
              [:text {:style {:color :ref/on-surface}} title]
              [:text {:style {:color :ref/background-overlay-4}} "\u251C"]]
      children)]))

(g/defcomponent OutsetPanel
  [props _]
  (let [{:keys [title border children]} props]
    (log/info "Panel props" props)
    (log/info "Panel children" children)
    [:view {:style {:border (or border 1)
                    :border-color-top :ref/surface-overlay-4
                    :border-color-left :ref/surface-overlay-4
                    :border-color-bottom :ref/background-overlay-4
                    :border-color-right :ref/background-overlay-4
                    :background-color :ref/surface}}
      (cons [:view {:key "panel-title"
                    :style {:position :absolute
                            :height 1 :width (+ 2 (count title))
                            :top -1 :left 1
                            :display :flex
                            :flex-direction :row}}
              [:text {:style {:color :ref/background-overlay-4}} "\u2524"]
              [:text {:style {:color :ref/on-surface}} title]
              [:text {:style {:color :ref/background-overlay-4}} "\u251C"]]
      children)]))
;; style taken from https://www.nucleo.com.au/using-flexbox-for-modal-dialogs/
(def Popup (zc/create-react-class {
  :display-name "Popup"
  :render (fn [this]
    (log/trace "Popup render")
    (let [{:keys [style children] :as props} (zc/props this)]
      (zc/csx [:view {:style {:max-height "100%" :max-width "100%"
                              :height "100%"
                              :align-items :center
                              :justify-content :center
                              :position :fixed
                              :top 0 :left 0
                              :color (zcolor/color 0 0 0 128)
                              :background-color (zcolor/color 0 0 0 128)}} [
                [:view {:style (merge
                                 {:color (zcolor/color 255 255 255 255)
                                  :background-color (zcolor/color 0 0 0 255)
                                  :margin-top 10
                                  ;:margin-bottom 10
                                  :padding 0
                                  :border 1 :border-style :single
                                  :max-height "90%"
                                  :max-width "90%"}
                                style)}
                    children]]])))}))

(defn interpolate-to [start end]
  (fn [steps]
    (let [interval (- end start)
          step (/ interval steps)]
      (range start end step))))

