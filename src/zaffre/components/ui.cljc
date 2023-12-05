(ns zaffre.components.ui
  (:require
    [overtone.at-at :as atat]
    [clj-http.client :as client]
    [clojure.java.io :as jio]
    [clojure.core.cache :as cache]
    [clojure.pprint :as pprint]
    [taoensso.timbre :as log]
    rockpick.core
    [zaffre.color :as zcolor]
    [zaffre.components :as zc]
    [zaffre.font :as zfont]
    [zaffre.imageutil :as ziu]))

(log/set-level! :info)

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

(def InputSelect (zc/create-react-class {
  :display-name "InputSelect"
  :get-initial-state (fn []
    {:index 0})
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
        (first children)))}))
  
(def Input (zc/create-react-class {
    :display-name "Input"
    :get-initial-state (fn []
                         (log/info "Input get-initial-state" (zc/element-id-str zc/*current-owner*))
                         (assert (not (nil? zc/*current-owner*)) "Input *current-owner* nil")
                         (assert (not= (zc/element-id-str zc/*current-owner*) "null") "Input *current-owner* null")
                         {:value ""
                          :show-cursor false
                          :focused false})
    :get-derived-state-from-props (fn [this next-props next-state]
                                    (select-keys next-props [:value :focused]))
    :get-default-props (fn input-get-default-props [] {
      :max-length 28
      :style {:width 30
              :height 1
              :display :flex
              :flex-direction :row
              :cursor-char-on \u2592
              :cursor-char-off \space
              :cursor-fg (zcolor/color 255 255 255 255)
              :cursor-bg (zcolor/color 0 0 0 255)}
      :on-focus (fn [this e]
                  (zc/set-state! this (fn [s] (merge s {:focused true}))))
      :on-blur (fn [this e]
                  (zc/set-state! this (fn [s] (merge s {:focused false}))))
      :on-keypress (fn input-on-keypress [this e]
                     (log/info "Input on-keypress" e)
                     (let [{:keys [max-length]} (zc/props this)
                           k (get e :key)]
                       (cond
                         (= k :backspace)
                             (zc/set-state! this (fn [{:keys [value]}]
                                                   {:value (subs value 0 (dec (count value)))}))
                         (and (or (char? k) (= k :space)) (not= k \newline))
                           (zc/set-state! this (fn [{:keys [value]}]
                                                 (if (< (count value) max-length)
                                                   {:value (str value (to-input-char k))}
                                                   {:value value}))))))})
    :render
      (fn [this]
        (let [{:keys [value focused]} (zc/state this)
              {:keys [style] :as props} (zc/props this)
              prop-value (get props :value)
              value (or prop-value value)
              {:keys [width
                      cursor-char-on cursor-char-off
                      cursor-fg cursor-bg]}  style
              duty-on 400
              duty-off 400
              t (mod (System/currentTimeMillis) (+ duty-on duty-off))
              show-cursor (< t duty-on)
              cursor (if (and focused show-cursor) cursor-char-on cursor-char-off)]
          (log/debug "Input render" show-cursor (dissoc props :children))
          (zc/csx [:view {:style style} [
                    [:text {} [value]]
                    [:text {:style {:color cursor-fg :background-color cursor-bg}} [(str cursor)]]
                    [:text {} [(apply str (repeat (- width (count value)) "_"))]]]])))}))

(def FileResource (zc/create-react-class {
  :display-name "FileResource"
  :get-initial-state (fn [] {:state :not-loaded})
  :component-will-mount (fn [this]
    (log/info "FileResource component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [src child-type]} (zc/props this)]
      (zc/set-state! this {:state :loading})
      (future
        (try
          (binding [zc/*current-owner* owner
                    zc/*updater* updater]
            (log/info "FileResource component-will-mount load-fn " src " " (zc/element-id-str zc/*current-owner*))
            (let [bytes (get (swap! resource-cache
                                    cache/through-cache
                                    src
                                    (constantly (ziu/slurp-bytes (jio/as-file src))))
                             src)]
              (log/info "FileResource loaded " (count bytes) " bytes" (zc/element-id-str zc/*current-owner*))
              (zc/set-state! this {:state :loaded :data bytes})))
          (catch Exception e
            (log/error e))))))
  :render (fn [this]
    {:post [(zc/element? %)]}
    (let [{:keys [src render]} (zc/props this)
          {:keys [state data]} (zc/state this)]
      (log/trace "File Resource render" src state)
      (case state
         :not-loaded
           (zc/csx [:text {} ["not loaded"]])
         :loading
           (zc/csx [:text {} ["loading"]])
         :loaded
           (render data))))}))

(def UrlResource (zc/create-react-class {
  :display-name "UrlResource"
  :get-initial-state (fn [] {:state :not-loaded})
  :component-will-mount (fn [this]
    (log/info "UrlResource component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [src child-type]} (zc/props this)]
      #_(zc/set-state! this {:state :loading})
      (future
        (try
          (binding [zc/*current-owner* owner
                    zc/*updater* updater]
            (log/info "UrlResource component-will-mount load-fn" (zc/element-id-str owner))
            (let [bytes (get (swap! resource-cache
                                    cache/through-cache
                                    src
                                    (constantly (client/get src {:as :byte-array})))
                             src)]
              (zc/set-state! this {:state :loaded :data bytes})))
          (catch Exception e
            (log/error e)
            (assert false))))))
  :render (fn [this]
    (let [{:keys [src render]} (zc/props this)
          {:keys [state data]} (zc/state this)]
      (case state
         :not-loaded
           (zc/csx [:text {} ["not loaded"]])
         :loading
           (zc/csx [:text {} ["loading"]])
         :loaded
           (render data))))}))


(def Resource (zc/create-react-class {
  :display-name "Resource"
  :render (fn [this]
    (let [{:keys [src render]} (zc/props this)]
      (log/trace "Resource render" src)
      (if (clojure.string/starts-with? src "http")
        (zc/csx [UrlResource {:src src :render render}])
        (zc/csx [FileResource {:src src :render render}]))))}))
  
(def NativeImage (zc/create-react-class {
  :display-name "NativeImage"
  :get-initial-state (fn []
                       (log/info "NativeImage get-initial-state")
                       {:state :not-loaded})
  :component-will-mount (fn [this]
    (log/info "NativeImage component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [src child-type]} (zc/props this)]
      #_(zc/set-state! this {:state :loading})
      (future
        (try
          (binding [zc/*current-owner* owner
                    zc/*updater* updater]
            (log/info "NativeImage component-will-mount load-fn" (zc/element-id-str owner))
            (let [{:keys [data style]} (zc/props this)
                  clip (get style :clip)
                  k [data clip]
                  child (try
                          (get (swap! image-cache
                                    cache/through-cache
                                    k
                                    (constantly 
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
                                          (zc/csx [:img {:width width :height (/ height 2)}
                                                   img-characters])))))
                             k)
                            (catch Exception e
                              (log/error e)
                              (assert false)))]
              (zc/set-state! this {:state :loaded :child child})))
          (catch Exception e
            (log/error e)
            (assert false))))))
  :render (fn [this]
    (let [{:keys [src render]} (zc/props this)
          {:keys [state child]} (zc/state this)]
      (case state
         :not-loaded
           (zc/csx [:text {} ["not loaded"]])
         :loading
           (zc/csx [:text {} ["loading"]])
         :loaded
           child)))}))

(def RexPaintImage (zc/create-react-class {
  :display-name "RexPaintImage"
  :render (fn [this]
    (let [{:keys [data layer-index]} (zc/props this)
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
      (zc/csx [:img {:width (count (first pixels)) :height (count pixels)} pixels])))}))

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

(def DataImage  (zc/create-react-class {
  :display-name "DataImage"
  :render (fn [this]
    (let [{:keys [src data style]} (zc/props this)]
      (if (clojure.string/ends-with? src "xp")
        (zc/csx [RexPaintImage {:data data :style style}])
        (zc/csx [NativeImage {:data data :style style}]))))}))
  
(def Image (zc/create-react-class {
  :display-name "Image"
  :render (fn [this]
    (let [props (assoc (zc/props this)
                  :render (fn [data]
                            {:post [(zc/element? %)]}
                            (let [props (assoc (zc/props this) :data data)]
                              (log/trace "Image render-prop fn" (get props :src))
                              (zc/csx [DataImage props]))))]
      (log/trace "Image render" (get props :src))
      ;; passes :src :style, etc through
      (zc/csx [Resource props []])))}))

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

