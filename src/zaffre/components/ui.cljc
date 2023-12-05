(ns zaffre.components.ui
  (:require
    [overtone.at-at :as atat]
    [cashmere.core-graal :as cm]
    [clj-http.client :as client]
    [clojure.zip :as zip]
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

(cm/defcomponent Input
  [props _]
  (let [[value set-value!] (cm/use-state (get props :value ""))
        [focused set-focus!] (cm/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (cm/use-state false)
        max-length (or (get props :max-length) 28)
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [e]
          #_(log/info "Input on-keypress" e)
          (let [k (get e :key)]
            #_(log/info "value" value "max-length" max-length "k" k)
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

     (cm/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

     #_(log/info "Input value" value (type value) (count value))
     #_(log/info "props" props)
     #_(log/debug "Input render" show-cursor (dissoc props :children))
     #_(log/info "Input width" width (type width))
     #_(log/info "Input rest" (apply str (repeat (- width (count value)) "_")))
     #_(log/info "cursor fg" cursor-fg "cursor-bg" cursor-bg "cursor" cursor)
     [:view props
       [:text {:key "value"} (str value)]
       [:text {:key "cursor" :style {:color cursor-fg :background-color cursor-bg}} (str cursor)]
       [:text {:key "rest"} (apply str (repeat (- width (count value)) "_"))]]))

(cm/defcomponent Button
  [props _]
  (let [[focused set-focus!] (cm/use-state (get props :focus false))
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

(cm/defcomponent Checkbox
  [props _]
  (let [[selected set-selected!] (cm/use-state (get props :checked false))
        [focused set-focus!] (cm/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (cm/use-state false)
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [e]
          #_(log/info "Checkbox on-keypress" e)
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
     (cm/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

    [:view props
      [:text {:key "checkbox-left"
              :style {:content :ref/checkbox-left
                      :color :ref/on-surface}} ""]
      [:text {:key "checkbox-check"
              :style {:content cursor
                      :color color}} ""]
      [:text {:key "checkbox-right"
              :style {:content :ref/checkbox-right
                      :color :ref/on-surface}} ""]
      children]))

(cm/defcomponent LoadingSpinner
  [props _]
  (let [{:keys [speed characters] :or {speed 100 characters ["/" "-" "\\" "|"]}} props
        [index set-index!] (cm/use-state 0)]
    (cm/use-effect (fn []
      (go-loop []
        (<! (timeout speed))
        (set-index! (fn [index] 
          (mod (inc index) (count characters))))
        (recur))) [])
    [:text {:key (get props :key)
            :style {:color :ref/on-surface}} (nth characters index)]))

(cm/defcomponent Radio
  [props _]
  (let [[selected set-selected!] (cm/use-state (get props :checked false))
        [focused set-focus!] (cm/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (cm/use-state false)
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
     (cm/use-effect (fn []
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

(cm/defcomponent ProgressBar
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

(cm/defcomponent Slider
  [props _]
  (let [initial-value (int (get props :initial-value 0))
        [value set-value!] (cm/use-state initial-value)
        [focused set-focus!] (cm/use-state (get props :focus false))
       [show-cursor set-show-cursor!] (cm/use-state false)
        {:keys [name style]} props
        {:keys [width] :or {width 40}} style
        duty-on 400
        duty-off 400
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        on-keypress (fn [e]
          #_(log/info "Slider on-keypress" (keys e))
          (let [increment (int (/ 100 width))]
            (case (get e :key)
              :left (set-value! (fn [value] (max 0 (- value increment))))
              :right (set-value! (fn [value] (min 100 (+ value increment)))))))
        left (int (* width (/ value 100)))
        right (- width left)
        default-props {:on-focus on-focus
                       :on-blur on-blur
                       :on-keypress on-keypress
                       :gossamer.components/type :slider
                       :gossamer.components/focusable true
                       :gossamer.components/set-value set-value!
                       :style {:display :flex
                               :flex-direction :row
                               :height 1
                               :width width
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
        cursor (if (and focused show-cursor) (str cursor-char-on) :ref/slider-control)
        color :ref/secondary]
     (cm/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])

    ; FIXME copy key (props?) into view
    [:view props
      [:view {:key "left"
              :style {:background-char :ref/slider-filled
                      :color :ref/secondary
                      :width (str left "%")
                      :height 1}}]
      [:text {:key "control" :style {:content cursor
                                     :color :ref/secondary}} ""]
      [:view {:key "right"
              :style {:background-char :ref/slider-empty
                      :color :ref/on-surface
                      :width (str right "%")
                      :height 1}}]]))

; Adapted from https://stackoverflow.com/questions/40153970/transpose-a-list-of-lists-clojure
(defn transpose [xs]
  (apply mapv vector xs))

(defn- merge-slice
  [slice1 slice2]
  (map (fn [char1 char2]
         (if (= (:c char1) \space)
           char2
           char1))
       slice1 slice2))

(defn map-vals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn format-line-data
  [series width height]
  ; series is a map from fg to data
  (assert (pos? (count series)))
  #_(log/info "formatting" series)
  (let [min-y (reduce min (map (fn [s] (reduce min s)) (vals series)))
        max-y (reduce max (map (fn [s] (reduce max s)) (vals series)))
        max-y (if (= min-y max-y) (inc max-y) max-y)
        ; characters
        star {:c \*
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        blank {:c \space
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        horizontal {:c \u2500
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        vertical {:c \u2502
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        tr {:c \u2510
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        tl {:c \u250C
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        br {:c \u2518
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        bl {:c \u2514
               :fg (zcolor/color 255 255 255)
               :bg (zcolor/color 0 0 0)}
        ; scale from [min-y max-y] to [0 height]
        scaled-data (map-vals (fn [data]
                           (map (fn [y] (zcolor/lerp 1 height (/ (- y min-y) (- max-y min-y)))) data))
                         series)
        series-pairs (take width
                       (map (fn [[fg series-scaled-data]]
                              (map (fn [& pair]
                                     [fg pair])
                                   (cons (first series-scaled-data) series-scaled-data)
                                   series-scaled-data))
                            scaled-data))]
    #_(log/info "width" width "height" height "data" (vec scaled-data))
    #_(log/info "min-y" min-y "max-y" max-y)
    #_(log/info "scaled-data" (vec scaled-data))
    #_(log/info "pairs" (vec series-pairs))
    ; for all vertical slices
    ; TODO: modify to operator over serieses
    ; for each vertical slice, reduce over series
    (->> series-pairs
      transpose
      (mapv (fn [pairs]
        (->> pairs
          (mapv (fn [[fg [prev-y y]]]
            ; draw a vertical slice
            (let [lower (int (min prev-y y))
                  upper (int (max prev-y y))
                  line (concat
                         (repeat (- height upper) blank)
                         (if
                           ; no change
                           (= (int upper) (int lower))
                             [horizontal]
                           (let [[top-char bottom-char] (if (< prev-y y)
                                                          [tl br]
                                                          [tr bl])]
                             (concat
                               [top-char]
                               (repeat (- upper
                                          lower 1) vertical)
                               [bottom-char])))
                         (repeat (inc lower) blank))]
                (map (fn [c] (assoc c :fg fg)) line))))
          (reduce merge-slice)))))))

(defn -main [& args]
  (let [data (format-line-data
               {:fg1 (map (partial * 1.) (range 16))
                :fg2 (map (fn [y]  (+ 4 (* -1. y))) (range 16))}
               16 8)]
    (doseq [line data]
      (println (apply str (map :c line))))
    (doseq [line (transpose data)]
      (println (apply str (map :c line))))))

;; Dummy component for XYPlot
(cm/defcomponent LineSeries
  [_ _])

(cm/defcomponent XYPlot
  [props _]
  "[XYPlot {}
     [LineSeries {:style {:color ...} :data [0 1 2 3]}]
     [LineSeries {:style {:color ...} :data [4 3 2 1]}]]"
  (let [{:keys [min-y max-y style children]} props
        style-width (get style :width 0)
        style-height (get style :height 0)
        [width set-width!] (cm/use-state style-width)
        [height set-height!] (cm/use-state style-height)
        r (cm/use-ref nil)
        img-characters (cm/use-memo
          ; TODO: create img-characters based on data, width, height
          (fn []
            ; scale data from [min-y, max-y] to [0, height]
            (format-line-data (into {}
                                (map (fn [child]
                                       (let [child-props (nth child 1)]
                                       [(get-in child-props [:style :color] (zcolor/color 255 255 255))
                                        (get child-props :data)]))
                                     children))
                              width height))
          [width height children])]
    #_(cm/use-effect (fn []
      (log/info "ref" r)
      (when-let [layout (some-> r (get "current") (nth 3) deref)]
        (let [{:keys [x y width height]} layout]
          (set-width! width)
          (set-height! height))))
      [r #_(get r "current")])

    #_(log/info "plotting data" children)
    #_(log/info "image-characters" (transpose img-characters))
    #_(System/exit 1)

    [:view {:ref r
            :style {:display :flex
                    :flex-direction :column
                    :width width
                    :justify-content :space-between
                    :height height}}
     [:img {:width width
            :height height
            :pixels (transpose img-characters)}]]))

(cm/defcomponent Dropdown
  [props _]
  (let [{:keys [children]} props
        [index set-index!] (cm/use-state 0)
        [focused set-focus!] (cm/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (cm/use-state false)
        [expanded set-expanded!] (cm/use-state false)
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_]
                  (set-focus! false)
                  (set-expanded! false))
        on-keypress (fn [e]
                      (if expanded
                        (case (:key e)
                          :up (set-index! (fn [idx] (max 0 (dec idx))))
                          :down (set-index! (fn [idx] (min (dec (count children)) (inc idx))))
                          (:enter :space)
                            (set-expanded! not)
                          nil)
                        (set-expanded! not)))
        default-props {:on-focus on-focus
                       :on-blur on-blur
                       ; TODO Implement arrow up/down
                       :on-keypress on-keypress
                       :gossamer.components/type :dropdown
                       :gossamer.components/focusable true
                       :style {:width 30
                               :height 1
                               :display :flex
                               :flex-direction :row
                               :cursor-char-on \u2592
                               :cursor-char-off \u25BC
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
     (cm/use-effect (fn []
       (go-loop []
         (<! (timeout 400))
         (set-show-cursor! not)
         (recur))) [])
    #_(log/info "cursor" cursor)
    #_(log/info "fg" cursor-fg)

    (conj [:view props]
      (concat
        [(nth children index)
         [:text {:key "cursor"
                 :style {:color cursor-fg}} (str cursor)]]
        (if expanded
          [[:view {:key "menu"
                   :style {:position :absolute
                           :top 1}}
            (map-indexed (fn [idx child]
               (if (= idx index)
                 (-> child
                   (assoc-in [1 :style :color] :ref/surface)
                   (assoc-in [1 :style :background-color] :ref/secondary))
                 child))
               children)]]
          [])))))

; Dummy component for Tree
(cm/defcomponent TreeItem
  [_ _])
;; Primary for implementation
(cm/defcomponent TreeItemImpl
  [props _]
  (let [{:keys [key edges label has-children show-cursor]} props
        [focused set-focus!] (cm/use-state false)
        on-focus (fn [_] (set-focus! true))
        on-blur (fn [_] (set-focus! false))
        [display set-display!] (cm/use-state true)
        toggle-tree-item (if has-children
                             (fn [e] 
                               (set-display! not))
                           identity)
        content (if has-children
                  (if (and focused show-cursor)
                      :ref/cursor
                    (if display
                      :ref/tree-item-expanded
                      :ref/tree-item-collapsed))
                  nil)]
     (when has-children
       (log/info "focused" focused "show-cursor" show-cursor "content" content))
     ; return a view
     [:view {:key key
             :style {:display :flex
                     :flex-direction :row}}
       ; proceeded by tree edges
       ; omit first edge
       [:text {:key "edges"} (subs edges 1)]
       ; follow by + or - button
       [:text {:key "button"
               :on-click toggle-tree-item
               :on-focus on-focus
               :on-blur on-blur
               ;:on-keypress on-keypress
               :gossamer.components/focusable has-children
               :style {:color :ref/on-secondary
                       :background-color :ref/secondary
                       :content content}} ""]
       ; followed by item label
       [:text {:key "label"} label]]))

(defn tree-loc
  [root-component]
  (zip/zipper
    (fn [component]
      (and (vector? component)
           (< 2 (count component))
           (= TreeItem (first component))))
    (fn [component]
      (drop 2 component))
    (fn [component children]
      (vec (concat (take 2 component) children)))
    root-component))

(cm/defcomponent Tree
  [props _]
  (let [{:keys [children]} props
        ;[focused set-focus!] (cm/use-state (get props :focus false))
        [show-cursor set-show-cursor!] (cm/use-state false)
        root-loc (tree-loc (first children))
        descendant-zippers (zc/zipper-descendants root-loc)
        tree-edges (map (comp clojure.string/join zc/tree-edges) descendant-zippers)]
    #_(log/info "tree-root" (first children))
    #_(log/info "tree items" (count items) (vec items))
    #_(log/info "tree edges" (count tree-edges) (vec tree-edges))
    (cm/use-effect (fn []
      (go-loop []
        (<! (timeout 400))
        (set-show-cursor! not)
        (recur))) [])
    (let [[elements skip] (reduce
                            (fn [[elements skip] [edges item-loc]]
                                 #_(log/info "tree-item" edges item)
                              (if (zero? skip)
                                [(cons
                                   (let [item (zip/node item-loc)
                                         has-children (< 2 (count item))]
                                   
                                     [TreeItemImpl {:key (get-in item [1 :key])
                                                    :show-cursor show-cursor
                                                    :edges edges
                                                    :label (get-in item [1 :label] "")
                                                    :has-children has-children}])
                                   elements)
                                 0]
                                [elements (dec skip)]))
                            ; [elements skip]
                            [(list) 0]
                            (map vector
                              tree-edges
                              descendant-zippers))]
      (log/info "Rendering Tree")
      (log/info "tree elements" elements)

      [:view {:key (get props :key)
              :style {:display :flex
                      :flex-direction :column}}
        ; reverse since list cons-es head first
        ; and a vector will not be expanded by hiccup
        (reverse elements)])))

#_(cm/defcomponent VScrollBar
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

(cm/defcomponent ScrollPane
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
  (let [[data set-data!] (cm/use-state nil)
        [error set-error!] (cm/use-state nil)
        [is-loading set-is-loading!] (cm/use-state true)]
    (cm/use-effect (fn effect-fn []
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

(cm/defcomponent NativeImage
  [props _]
  (let [{:keys [src data style]} props
        clip (get style :clip)
        [width height img-characters :as memo] (cm/use-memo
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

(cm/defcomponent RexPaintImage
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

#_(def ListImage (zc/create-react-class {
  :display-name "ListImage"
  :render (fn [this]
    (let [{:keys [data]} (zc/props this)
          width (reduce max (map count data))
          height (count data)
          pixels (data-to-pixels data)]
      (log/trace "img-characters" pixels)
      (zc/csx [:img {:width width :height (/ height 2)} pixels])))}))

(cm/defcomponent DataImage
  [props _]
  (let [{:keys [src data style]} props]
    (if (clojure.string/ends-with? src "xp")
      [RexPaintImage {:src src :data data :style style}]
      [NativeImage {:src src :data data :style style}])))
  
(cm/defcomponent Image
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

(cm/defcomponent Panel
  [props _]
  (let [{:keys [title border children]} props]
    #_(log/info "Panel props" props)
    #_(log/info "Panel children" children)
    [:view {:key (get props :key)
            :style {:border (or border 1)
                    :background-color :ref/surface}}
      (cons [:text {:key "panel-title"
                    :style {:position :absolute
                            :height 1 :width (+ 2 (count title))
                            :top -1 :left 1}}
             (str "\u2524" title "\u251C")]
      children)]))

(cm/defcomponent InsetPanel
  [props _]
  (let [{:keys [title border children]} props]
    #_(log/info "Panel props" props)
    #_(log/info "Panel children" children)
    [:view {:key (get props :key "inset-panel")
            :style {:border (or border 1)
                    :border-style :single
                    :border-color-top :ref/background-overlay-4
                    :border-color-left :ref/background-overlay-4
                    :border-color-bottom :ref/surface-overlay-4
                    :border-color-right :ref/surface-overlay-4
                    :background-color :ref/surface}}
      children
      #_(cons [:view {:key "panel-title"
                    :style {:position :absolute
                            :height 1 :width (+ 2 (count title))
                            :top -1 :left 1
                            :display :flex
                            :flex-direction :row}}
              [:text {:key "left" :style {:color :ref/background-overlay-4}} "\u2524"]
              [:text {:key "title" :style {:color :ref/on-surface}} title]
              [:text {:key "right" :style {:color :ref/background-overlay-4}} "\u251C"]]
      children)]))

(cm/defcomponent OutsetPanel
  [props _]
  (let [{:keys [title border children]} props]
    #_(log/info "Panel props" props)
    #_(log/info "Panel children" children)
    [:view {:key (get props :key)
            :style {:border (or border 1)
                    :border-style :single
                    :border-color-top :ref/surface-overlay-4
                    :border-color-left :ref/surface-overlay-4
                    :border-color-bottom :ref/background-overlay-4
                    :border-color-right :ref/background-overlay-4
                    :background-color :ref/surface}}
      children
      #_(cons [:view {:key "panel-title"
                    :style {:position :absolute
                            :height 1 :width (+ 2 (count title))
                            :top -1 :left 1
                            :display :flex
                            :flex-direction :row}}
              [:text {:key "left" :style {:color :ref/background-overlay-4}} "\u2524"]
              [:text {:key "title" :style {:color :ref/on-surface}} title]
              [:text {:key "right" :style {:color :ref/background-overlay-4}} "\u251C"]]
      children)]))
;; style taken from https://www.nucleo.com.au/using-flexbox-for-modal-dialogs/
#_(def Popup (zc/create-react-class {
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
                                  :border 1
                                  :border-style :single
                                  :max-height "90%"
                                  :max-width "90%"}
                                style)}
                    children]]])))}))

(defn interpolate-to [start end]
  (fn [steps]
    (let [interval (- end start)
          step (/ interval steps)]
      (range start end step))))

