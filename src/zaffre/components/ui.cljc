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

(declare Input)
(declare InputSelect) 

(defn input-element-seq [dom]
  (zc/filter-element-tree (fn [{:keys [type]}]
                            (= type Input))
                          dom))

(defn input-select-element [dom]
  (first
    (zc/filter-element-tree (fn [{:keys [type]}]
							  (= type InputSelect))
							dom)))

(defn to-input-char [event]
  (if (= event :space)
    \ 
    event))

#_(g/defcomponent InputSelect
  [_ _]
  (let [[state set-state] (g/use-state {:index 0})]
  :get-default-props (fn input-get-default-props [] {
    :on-keypress (fn input-select-on-keypress [this e]
                   (log/info "InputSelect on-keypress" (get e :key))
                   (let [{:keys [key dom]} e
                         {:keys [index]} (zc/state this)
                         input-elements (input-element-seq dom)]
                     (when-not (empty? input-elements)
                       (let [curr-input (nth input-elements (mod index (count input-elements)))]
                         (if (= key :tab)
                           (let [next-input (nth input-elements (mod (inc index) (count input-elements)))]
                             (when-not (= curr-input next-input)
                               ;; blur curr input
                               (binding [zc/*current-owner* curr-input]
                                 (let [instance (zc/construct-instance curr-input)
                                     {:keys [on-blur]} (zc/props instance)]
                                   (on-blur
                                     (assoc instance :updater zc/*updater*)
                                     {})))
                               ;; focus next input
                               (binding [zc/*current-owner* next-input]
                                 (let [instance (zc/construct-instance next-input)
                                     {:keys [on-focus]} (zc/props instance)]
                                   (on-focus
                                     (assoc instance :updater zc/*updater*)
                                     {})))
                               ;; update this state
                               (zc/set-state! this (fn [{:keys [index]}]
                                                     {:index (inc index)}))))
                            ;; dispatch event to curr input
                            (binding [zc/*current-owner* curr-input]
                              (let [instance (zc/construct-instance curr-input)
                                  {:keys [on-keypress]} (zc/props instance)]
                                (on-keypress
                                  (assoc instance :updater zc/*updater*)
                                  {:key key}))))))))})
  :render
    (fn [this]
      (let [{:keys [children] :as props} (zc/props this)]
        (log/trace "InputSelect render")
        (first children)))))
  
(g/defcomponent Input
  [props _]
  (let [[value set-value!] (g/use-state (get props :value ""))
        [focused set-focus!] (g/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (g/use-state false)
        duty-on 400
        duty-off 400
        default-props {:max-length 28
                       :style {:width 30
                               :height 1
                               :display :flex
                               :flex-direction :row
                               :cursor-char-on \u2592
                               :cursor-char-off \space
                               :cursor-fg (zcolor/color 255 255 255 255)
                               :cursor-bg (zcolor/color 0 0 0 255)}}
        props (assoc (merge default-props props)
                :style (merge (:style props)
                              (:style default-props)))]

     (g/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

     #_(log/info "Input value" value (type value))
     #_(log/info "props" props)
     (letfn [(on-focus [_] true)
             (on-blur [_] false)
             (on-keypress [e]
                (log/info "Input on-keypress" e)
                (let [{:keys [max-length]} props
                      k (get e :key)]
                  (cond
                    (= k :backspace)
                      (set-value! (subs value 0 (dec (count value))))
                    (and (or (char? k) (= k :space)) (not= k \newline))
                      (set-value! (if (< (count value) max-length)
                                    (str value (to-input-char k))
                                    value)))))]
        (let [{:keys [style]} props
              {:keys [width
                      cursor-char-on cursor-char-off
                      cursor-fg cursor-bg]}  style
              cursor (if (and focused show-cursor) cursor-char-on cursor-char-off)]
          #_(log/debug "Input render" show-cursor (dissoc props :children))
          #_(log/info "Input width" width (type width))
          #_(log/info "cursor fg" cursor-fg "cursor-bg" cursor-bg "cursor" cursor)
          [:view {:style style
                  :class :tabable
                  :on-focus on-focus
                  :on-blur on-blur
                  :on-keypress on-keypress}
            [:text {:key "text"} value]
            [:text {:key "cursor" :style {:color cursor-fg :background-color cursor-bg}} (str cursor)]
            [:text {:key "rest"} (apply str (repeat (- width (count value)) "_"))]]))))

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

