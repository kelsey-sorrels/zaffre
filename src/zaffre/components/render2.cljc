(ns zaffre.components.render2
  (:require clojure.data
            [clojure.zip :as zip]
            [clojure.core.async :as a :refer [buffer chan go-loop <! >! >!!]]
            [clojure.string]
            [clojure.contrib.def]
            [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [cashmere.core-graal :as cm]
            #_[cashmere.template :as cmt]
            #_[cashmere.element :as cme]
            [zaffre.components.layout :as zl]
            [zaffre.color :as zcolor]
            [zaffre.text :as ztext]
            [zaffre.terminal :as zt]
            [zaffre.util :as zu]))

(def default-style
  {:color (unchecked-int 0xffffffff) #_[255 255 255 255]
   :background-color (unchecked-int 0x000000ff) #_[0 0 0 255]
   :background-char \ 
   :border-style :single
   :border 0
   :border-left 0
   :border-right 0
   :border-top 0
   :border-bottom 0
   :border-color-left (zcolor/color 255 255 255)
   :border-color-right (zcolor/color 255 255 255)
   :border-color-top (zcolor/color 255 255 255)
   :border-color-bottom (zcolor/color 255 255 255)
   :mix-blend-mode (get zt/blend-mode->byte :normal)
   ;; Theme (Dark)
   ; backgrounds
   :primary (zcolor/color 187 134 252)
   :primary-variant (zcolor/color 54 0 179)
   :secondary (zcolor/color 3 218 196)
   :secondary-variant (zcolor/color 3 218 196)
   :background (zcolor/color 18 18 18)
   :surface (zcolor/color 18 18 18)
   :error (zcolor/color 207 102 121)
   ; overlay
   :background-overlay-4 (zcolor/overlay-percent (zcolor/color 0 0 0) 4)
   :surface-overlay-4 (zcolor/overlay-percent (zcolor/color 18 18 18) 8)
   ; text
   :on-primary (zcolor/color 0 0 0)
   :on-secondary (zcolor/color 0 0 0)
   :on-background (zcolor/color 255 255 255)
   :on-surface (zcolor/color 255 255 255)
   :on-error (zcolor/color 0 0 0)
   ; characters
   :button-left "<"
   :button-right ">"
   :radio-checked "*"
   :radio-unchecked " "
   :radio-left "("
   :radio-right ")"
   :checkbox-checked "x"
   :checkbox-unchecked " " 
   :checkbox-left "["
   :checkbox-right "]"
   :progress-filled " "
   :progress-empty "\u2592"
   :slider-filled "\u2500"
   :slider-empty "\u2500"
   :slider-control "\u258C"
})

;; Primitive elements
(def primitive-elements #{:terminal :group :layer :view :text :img})
(defn primitive? [component]
  (or (string? component)
      (contains? primitive-elements (first component))))

(def text-forbidden-styles
  #{:border
    :border-left
    :border-right
    :border-top
    :border-bottom
    :border-color
    :border-color-left
    :border-color-right
    :border-color-top
    :border-color-bottom
    :border-horizontal
    :border-vertical
    :padding
    :padding-left
    :padding-right
    :padding-top
    :padding-bottom
    :padding-horizontal
    :padding-vertical})

; https://stackoverflow.com/questions/5612302/which-css-properties-are-inherited
(def inheritable-styles #{
  :azimuth
  :background-color
  :border-collapse
  :border-spacing
  :caption-side
  :color
  :cursor
  :direction
  :elevation
  :empty-cells
  :font-family
  :font-size
  :font-style
  :font-variant
  :font-weight
  :font
  :letter-spacing
  :line-height
  :list-style-image
  :list-style-position
  :list-style-type
  :list-style
  :orphans
  :pitch-range
  :pitch
  :quotes
  :richness
  :speak-header
  :speak-numeral
  :speak-punctuation
  :speak
  :speech-rate
  :stress
  :text-align
  :text-indent
  :text-transform
  :visibility
  :voice-family
  :volume
  :white-space
  :widows
  :word-spacing
  ;; Theme
  ;; NOTE: Theme values should ALWAYS be inheirited so that they propogate
  ;; throughout the element tree
  :primary
  :primary-variant
  :secondary
  :secondary-variant
  :background
  :surface
  :error
  ; overlay
  :background-overlay-4
  :surface-overlay-4
  ; text
  :on-primary
  :on-secondary
  :on-background
  :on-surface
  :on-error
  ; characters
  :button-left
  :button-right
  :radio-checked
  :radio-unchecked
  :radio-left
  :radio-right
  :checkbox-checked
  :checkbox-unchecked
  :checkbox-left
  :checkbox-right
  :progress-filled
  :progress-empty
  :slider-filled
  :slider-empty
  :slider-control
  ;; Non-standard properties
  :overlay-percent
})

;; Same as Clojure's select-keys, but uses transient map for intermediate results.
;; merge-default-style and cascade-style are made much faster by using transients here.
;; FPS difference from 125 to 350
(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  {:added "1.0"
   :static true}
  [map keyseq]
    (loop [ret (transient {}) keys (seq keyseq)]
      (if keys
        (let [entry (. clojure.lang.RT (find map (first keys)))]
          (recur
           (if entry
             (conj! ret entry)
             ret)
           (next keys)))
        (with-meta (persistent! ret) (meta map)))))

(defprotocol PStyleMap
  (compute-style [this k])
  (merge-styles [this child]))

(defrecord StyleMap [parent m cache]
  PStyleMap
  (compute-style [this k]
    ; Look in cache for k
    (if-let [v (-> cache deref (get k))]
      v
      ; if not in cache, compute and place in cache
      ; try to resolve using m
      (let [v (or (get m k)
                   ; if not resolve using parent if parent exists and k is an inheiritable style
                   (when (and parent
                            (contains? inheritable-styles k))
                     (do
                       #_(log/info "computing style from parent" parent)
                       (compute-style parent k))
                     #_(log/info "Missed style" k)))
            ; If the resolved value is a reference
            v (if (and (qualified-keyword? v)
                       (= (namespace v) "ref"))
                ; if value is qualified with ref ex: :ref/background-color
                ; then resolve the ref by looking up the unqualified version the keyword
                (compute-style this (keyword (name v)))
                ; else, we found the value
                v)]
        ; because the value wasn't originally in the cache, store it and then return it
        (swap! cache assoc k v)
        v)))
  (merge-styles [this child]
    (assert (map? child))
    (assert (not (instance? StyleMap child)))
    (->StyleMap this child (atom {}))))

(defn style-map
  [m]
  (assert (map? m))
  (->StyleMap nil m (atom {})))

(defn resolve-value
  [style v]
  #_(log/info "resolving" v (qualified-keyword? v) (if (qualified-keyword? v) (namespace v) v))
  #_(log/info "using" style)
  (if (and (qualified-keyword? v)
           (= (namespace v) "ref"))
    ; if value is qualified with ref ex: :ref/background-color
    ; then resolve the ref by looking up the unqualified version the keyword
    (get style (resolve-value style (keyword (name v))))
    v))

(defn replace-content
  [[type props children host-dom requires-layout :as element] style-map]
  #_(log/info "replacing content for" type (get-in props [:content :style]) props )
  (if-let [content (compute-style style-map :content)]
       #_(log/info "resolved-content" content)
     [type props [content] host-dom requires-layout]
     [type
      props
      (map replace-content children)
      host-dom
      requires-layout]))

; bounds are [x1 y1 x2 y2]
(defn layout->bounds [{:keys [x y width height]}]
  [x y (+ x width) (+ y height)])

(defmacro between [lower-bound value upper-bound]
  `(and (< ~lower-bound ~value) (< ~value ~upper-bound)))

(defn render-string-into-container [target x y color background-color s blend-mode]
  {:pre [(number? x)
         (number? y)
         (or (integer? color) (vector? color))
         (or (integer? background-color) (vector? background-color))
         (string? s)]}
  ;(log/info "render-string-into-container" x y color background-color s)
  (when (between -1 y (zt/num-rows target))
    (let [max-x (int (zt/num-cols target))
          row (int y)]
      (loop [index 0 s s]
        (let [col (int (+ x index))
              c   (first s)]
          (when (and c 
                     (between -1 col max-x))
            (zt/set! target c blend-mode (int 0) color background-color col row)
            (recur (inc index) (rest s))))))))

(defn render-text-into-container [target text-element]
  {:pre [(= (first text-element) :text)
         (map? (second text-element))
         (not (empty? (last text-element)))]}
  #_(log/info "render-text-into-container" text-element)
  (let [[type {:keys [zaffre/layout] :as props} children _ _ style] text-element
        {:keys [x y width height]} layout
        text-align (or (compute-style style :text-align) :left)
        mix-blend-mode (or (compute-style style :mix-blend-mode) (zt/blend-mode->byte :normal))
        overlay-percent (or (compute-style style :overlay-percent) 0)
        text-element (if (every? string? children)
                       [:text props (map (fn [child] [:text {} [child]]) children)]
                       text-element)
        #_ (log/info "text-element" text-element)
        lines (ztext/word-wrap-text-tree width height text-element)]
    #_(log/trace "render-text-into-container lines" (vec lines))
    (zu/loop-with-index dy [line lines]
      (let [default-color (get default-style :color)
            default-background-color (get default-style :background-color)]
        (when (and (< dy height) (not (empty? line)))
          (log/trace "rendering line" (vec line))
          ;; remove inc? for spaces
          (let [but-last (butlast line)
                last-span (let [[s _ _] (last line)
                                s (clojure.string/trim s)]
                           [s
                            {:color (compute-style style :color)
                             :background-color (compute-style style :background-color)}
                            (count s)])
                line (concat but-last (list last-span))
                _ (log/trace "rendering line2" (vec line))
                span-lengths (map (fn [[s _ length]] length) line)
                line-length (reduce + 0 span-lengths)
                align-offset (case text-align
                               :left 0
                               :center (/ (- width line-length) 2)
                               :right (- width line-length))
                offsets (map (partial + align-offset) (cons 0 (reductions + span-lengths)))]
            #_(log/trace "line-length" line-length)
            #_(log/trace "lengths" (vec span-lengths))
            #_(log/trace "offsets" (vec offsets))
            (doseq [[dx [s {:keys [color background-color]} _]] (map vector offsets line)]
              (render-string-into-container
                target
                (+ x dx) (+ y dy)
                (or color default-color)
                (zcolor/overlay-percent (or background-color default-background-color) overlay-percent)
                (clojure.string/trim s)
                mix-blend-mode))))))))

(defn render-img-into-container [target img-element]
  {:pre [(= (first img-element) :img)
         (map? (second img-element))
         #_(not (empty? (last img-element)))]}
  #_(log/debug "render-img-into-container" img-element)
  (let [[type {:keys [style zaffre/layout pixels] :or {style {}} :as props} children] img-element
        {:keys [mix-blend-mode]} style
        mix-blend-mode (or mix-blend-mode (get default-style :mix-blend-mode))
        {:keys [x y width height overflow-bounds]} layout
        overflow-bounds (or overflow-bounds (layout->bounds layout))
        [overflow-min-x overflow-min-y overflow-max-x overflow-max-y] overflow-bounds
        overflow-max-x (int overflow-max-x)
        ;; bounds for overflow/target container (whichever is smaller)
        min-x (dec (max 0 overflow-min-x))
        min-y (dec (max 0 overflow-min-y))
        num-cols-target (zt/num-cols target)
        num-rows (int (zt/num-rows target))
        max-y (min overflow-max-y num-rows)
        lines pixels]
    #_(log/debug "render-img-into-container layout " layout)
    #_(log/trace "render-img-into-container lines" (vec lines))
    #_(assert mix-blend-mode)
    (zu/loop-with-index dy [line lines]
      (let [y (int y)
            dy (int dy)
            row (+ y dy)]
        (when (and (< dy height) (not (empty? line)) (< min-y row max-y))
          #_(log/trace "rendering line" (vec line))
          (zu/loop-with-index dx [{:keys [c fg bg] :as pixel} line]
            (assert (char? c) pixel)
            (assert (or (integer? fg) (vector? fg)))
            (assert (or (integer? bg) (vector? bg)))
            (let [max-x (min overflow-max-x num-cols-target)
                  x (int x)
                  dx (int dx)
                  col (int (+ x dx))]
              (when (< min-x col max-x)
                #_(log/info "rendering pixel x:" col " y:" row " " pixel mix-blend-mode " max-x:" max-x " max-y:" (zt/num-rows target))
                (zt/set!
                  target
                  (get pixel :c)
                  mix-blend-mode
                  (get pixel :palette-offset 0)
                  (get pixel :fg)
                  (get pixel :bg)
                  col row)))))))))

(defn element-seq
  [parent-style [type props children host-dom requires-layout :as element]]
  "Cascades style-maps to children. Linearizes elements."
  (let [element-style (merge-styles parent-style (get props :style {}))]
    (assert (= (count element) 5) element)
    (lazy-seq
      (cons
        ; add style-map to element
        (conj element element-style)
        ; include children if not :text or :img element
        (when-not (contains? #{:text :img} type)
          (mapcat (partial element-seq element-style)
                  children))))))

(defmulti render-component-into-container (fn [_ element] (first element)))

;; render text
(defmethod render-component-into-container :text
  [target text-element]
  #_(log/info "rendering text")
  (render-text-into-container target text-element))

;; render imgs
(defmethod render-component-into-container :img
  [target img-element]
  (render-img-into-container target img-element))

;; Border
(def single-border
  {:horizontal   \u2500
   :vertical     \u2502
   :top-left     \u250C
   :top-right    \u2510
   :bottom-left  \u2514
   :bottom-right \u2518})

(def double-border
  {:horizontal   \u2550
   :vertical     \u2551
   :top-left     \u2554
   :top-right    \u2557
   :bottom-left  \u255A
   :bottom-right \u255D})

;; render views
(defmethod render-component-into-container :view
  [target [type {:keys [zaffre/layout] :as props} _ _ _ style]]
  #_(log/info "rendering view")
  (let [{:keys [x y width height]} layout
        color (compute-style style :color)
        background-color (compute-style style :background-color)
        background-char (or (compute-style style :background-char) \space)
        border (compute-style style :border)
        border-style (compute-style style :border-style)
        border-top (or (compute-style style :border-top) border)
        border-bottom (or (compute-style style :border-bottom) border)
        border-left (or (compute-style style :border-left) border)
        border-right (or (compute-style style :border-right) border)
        border-color-top (compute-style style :border-color-top)
        border-color-bottom (compute-style style :border-color-bottom)
        border-color-left (compute-style style :border-color-left)
        border-color-right (compute-style style :border-color-right)
        mix-blend-mode (or (compute-style style :mix-blend-mode) (zt/blend-mode->byte :normal))]
    (assert width)
    (assert height)
    (assert x)
    (assert y)
    #_(log/info "view-border" border-style border-top border-color-top)
    #_(log/info "view porps style" (get props :style))
    ;; render background when set
    (when background-color
      (doseq [dy (range height)
              :let [color (or color (get default-style :color))]]
        #_(log/info "color" color "background-color" background-color)
        (assert (or (integer? background-color) (vector? background-color)))
        (render-string-into-container target x (+ y dy) color background-color (clojure.string/join (repeat width background-char)) mix-blend-mode)))
    ; render border when set
    (when border-style
      (let [border-map (case border-style
                         :single single-border
                         :double double-border)]
        ; render top
        (when (or (< 0 border) (< 0 border-top))
          (render-string-into-container target x y border-color-top background-color
            (str (when (or (< 0 border) (< 0 border-left))
                   (get border-map :top-left))
                 (clojure.string/join (repeat (- width (+ (or border border-left) (or border border-right)))
                                              (get border-map :horizontal)))
                 (when (or (< 0 border) (< 0 border-right))
                   (get border-map :top-right)))
             mix-blend-mode))
        ; render middle
        (doseq [dy (range (- height 2))]
          (when (or (< 0 border) (< 0 border-left))
            (render-string-into-container target x (+ y dy 1) border-color-left background-color (str (get border-map :vertical)) mix-blend-mode))
          (when (or (< 0 border) (< 0 border-right))
            (render-string-into-container target (+ x width -1) (+ y dy 1) border-color-right background-color (str (get border-map :vertical)) mix-blend-mode)))
        ; render bottom
        (when (or (< 0 border) (< 0 border-bottom))
          (render-string-into-container target x (+ y height -1) border-color-bottom background-color
            (str (when (or (< 0 border) (< 0 border-left))
                   (get border-map :bottom-left))
                 (clojure.string/join (repeat (- width (+ (or border border-left) (or border border-right)))
                                              (get border-map :horizontal)))
                 (when (or (< 0 border) (< 0 border-right))
                   (get border-map :bottom-right)))
            mix-blend-mode)))))
  nil)

;; Do nothing for :layer
(defmethod render-component-into-container :layer
  [_ component]
  nil)

;; die if not :layer nor :view nor :text
(defmethod render-component-into-container :default
  [_ component]
  (assert false (format "Found unknown component %s" component)))

(defn intersect-bounds [[ax1 ay1 ax2 ay2 :as a-bounds] [bx1 by1 bx2 by2 :as b-bounds]]
  (and
    (< ax1 bx2)
    (> ax2 bx1)
    (> ay1 by2)
    (< ay2 by1)))

; TODO: Could this be fixed merged into element-seq?
(defn inherit-overflow-bounds
  ([element]
    (inherit-overflow-bounds nil element))
  ([parent-bounds element]
    (let [[type props children host-dom requires-layout] element]
      (cond
        (or (= type :text) (= type :img))
          [type
           (if parent-bounds
             (assoc-in props [:zaffre/layout :overflow-bounds] parent-bounds)
             props)
           children
           host-dom
           requires-layout]
        (= (get-in props [:style :overflow]) :hidden)
          [type
           props
           (map (partial inherit-overflow-bounds (layout->bounds (get props :zaffre/layout))) children)
           host-dom
           requires-layout]
        :else
          [type
           props
           (map (partial inherit-overflow-bounds parent-bounds) children)
           host-dom
           requires-layout]))))

(defn zipper-elements
  [root-element]
  (zip/zipper
    vector?
    (fn [[_ _ children _]] children)
    (fn [[tag props _ host-dom requires-layout] children] [tag props children host-dom requires-layout])
    root-element))
  
(defn zipper-descendants
  [z]
  (when-not (zip/end? z)
    (lazy-seq
      (cons z (zipper-descendants (zip/next z))))))

(defn requires-layout?
  [element]
  ; Any descendant requires layout?
  (some
    (fn [element-loc]
      (let [requires-layout (nth element 4)]
        #_(when requires-layout
          (log/info "Changed element requires layout" element))
        requires-layout))
    (->> element zipper-elements zipper-descendants (map zip/node))))

(defn copy-layout
  [element]
  #_(log/info "copy layout" element)
  (if (vector? element)
    (-> element
      (assoc-in [1 :zaffre/layout] (-> element (nth 3) deref))
      (update 2 (partial map copy-layout)))
    element))

(defn layout-or-last-layout
  [element]
  (if (requires-layout? element)
    (do
      (log/info "calculating layout")
      (zl/layout-element element))
    (do
      #_(log/info "reusing layout")
      (copy-layout element))))

(defn render-layer-into-container
  [target layer style-map]
  {:pre [(some? layer)]}
  #_(log/info "Render layer into container")
  #_(log/trace "render-layer-into-container" layer)
  ; replace :content with style-values
  (let [layer-with-content (replace-content layer style-map)
        layer-en-place (layout-or-last-layout layer)
        ; linearize and add style-maps
        elements (->> layer-en-place
                   inherit-overflow-bounds
                   (element-seq style-map)
                   vec)]
    #_(log/info "render-layer-into-container layer" (zc/tree->str layer))
    #_(log/info "render-layer-into-container layer-en-place" layer-en-place)
    #_(log/info "render-layer-into-container elements" elements)
    #_(log/info "render-layer-into-container layer-en-place" (zc/tree->str layer-en-place))
    (doseq [element elements]
      #_(log/info "render-layer-into-container element" element)
      (if (first element)
        (render-component-into-container target element)
        (log/error "error rendering" element)))
    elements))

(defn log-layer [layer-id layer-container]
  (println "▓" layer-id "▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓")
  (doseq [row (range (zt/num-rows layer-container))]
    (doseq [col (range (zt/num-cols layer-container))
            :let [c (zt/get-char layer-container col row)]]
      (if (< 0 (int c) 255)
        (print c)
        (print \space)))
    (println "▓"))
  (println "▓" layer-id "▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓"))
  
(defn layer-info [group-info]
  "Given a terminal's group info, create a map layer-id->{:columns rows}."
  (into {}
    (mapcat identity
      (map (fn [{:keys [layers columns rows]}]
             (map (fn [layer-id]
                    [layer-id {:columns columns :rows rows}])
                  layers))
           (vals group-info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render to virtual dom ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Renders component into container. Does not call refresh! on container
(defn render-into-container
  "Renders element into target. If existing is supplied, appropriate component
   lifecycle handlers will be invoked. Response is rendered elements with a
   meta property :layout-elements containing component id and layout records."
  ([target element root-style-map]
    (render-into-container target nil element root-style-map))
  ([target existing root-element root-style-map]
    #_(log/info "render-into-container root-element" root-element)
    (let [group-info (zt/groups target)
          layer-info (layer-info group-info)
          [type props groups :as root-dom] root-element
          elements (atom (list))]
      #_(log/info "render-into-container existing" (zc/tree->str existing))
      #_(log/info "render-into-container rendered-element" (zc/tree->str root-element))
      #_(log/info "render-into-container root-dom" (zc/tree->str root-dom))
      #_(System/exit 1)
      #_(log/info "type" type)
      (assert (= type :terminal)
              (format "Root component not :terminal found %s instead" type))
      ;; for each group in terminal
      (doseq [[type {:keys [id pos] :as group-props} layers] groups
              :let [{:keys [columns rows]} (get group-info id)]]
        (assert (= type :group)
                (format "Expected :group found %s instead" type))
        ;(assert id (str "No id found for group" group-props))
        #_(log/info "rendering group" id)
        ;; update group pos
        (when pos
          (zt/alter-group-pos! target id pos))
        ;; for each layer in group
        (doseq [[type {:keys [id style]} :as layer] layers]
          (assert (= type :layer)
                  (format "Expected :layer found %s instead. %s" type (str layer)))
          ;; create a container to hold cells
          ;; TODO reuse buffers
          (let [layer-container (zt/array-buffers columns rows)
                ;; merge layer width height into layer's style
                {:keys [columns rows]} (get layer-info id)]
            #_(log/info "render-into-container - layer" id)
            ;; render layer into layer-container
            (swap! elements concat
              (render-layer-into-container
                 layer-container
                 (-> layer
                   ;; assign width and height to layer style
                   (assoc-in [1 :style :width] columns)
                   (assoc-in [1 :style :height] rows))
                 root-style-map))
            #_(log-layer id layer-container)
            ;; send layer changes to terminal
            (zt/put-layer! target id layer-container))))
      (with-meta root-element {:layout-elements @elements}))))

; Animation frame support
(defonce request-animation-frame-handlers (atom []))
(defn request-animation-frame
  [f]
  (swap! request-animation-frame-handlers conj f))

(defn leaf-elements
  [dom]
  (let [dom-type (get dom :type )]
    (when (contains? #{:view :text :img} dom-type)
        (log/info (update dom :props (fn [props] (dissoc props :children)))))
  (when-let [children (-> dom :props :children)]
    (doseq [child children]
      (leaf-elements child)))))

(defn in-element?
  [col row element]
  #_(log/info "in-element?" col row element)
  (let [minx (-> element second :zaffre/layout :x)
        miny (-> element second :zaffre/layout :y)
        width (-> element second :zaffre/layout :width)
        height (-> element second :zaffre/layout :height)
        maxx (+ minx width)
        maxy (+ miny height)]
  (and (<= minx col (dec maxx))
       (<= miny row (dec maxy)))))

(defn handle-click [layout-elements event]
  (let [{:keys [col row]} event]
    (when (and col row)
      (let [hits (filter (partial in-element? col row) (reverse layout-elements))]
        #_(clojure.inspector/inspect-tree layout-elements)
        (log/info col row)
        (log/info "hits:" (count hits))
        (doseq [hit hits]
          (when-let [on-click (-> hit second :on-click)]
            (on-click {:client-x (- col (-> hit second :zaffre/layout :x))
                       :client-y (- row (-> hit second :zaffre/layout :y))
                       :screen-x col
                       :screen-y row
                       :target hit})))))))

(defn handle-mouse-enter [layout-elements event]
  (let [{:keys [col row]} event
        hits (filter (partial in-element? col row) (reverse layout-elements))]
    (log/trace col row)
    (log/trace (count hits) "hits")
    (doseq [hit hits]
      (when-let [on-mouse-enter (-> hit second :on-mouse-enter)]
        (on-mouse-enter {:target hit})))))


(defn handle-mouse-leave [layout-elements event]
  (log/info "handle-mouse-leave" #_(vec layout-elements))
  (let [{:keys [col row]} event
        hits (filter (partial in-element? col row) (reverse layout-elements))]
    (log/trace (count hits) "hits")
    (log/trace col row)
    (doseq [hit hits]
      (when-let [on-mouse-leave (-> hit second :on-mouse-leave)]
        (on-mouse-leave {:target hit})))))

(defonce focus-index (atom -1))

(defn focusable-element-locs
  "Returns a sequence of all zippers pointing to focusable elements."
  [root-element]
  (->> root-element
    zipper-elements
    zipper-descendants
    (filter (fn [z]
      (let [element (zip/node z)]
        (and (vector? element)
             (< 2 (count element))
             (let [[_ props _] element
                   focusable (get props :gossamer.components/focusable)]
                focusable)))))))

(defn radio-group
  [radio]
  (-> radio second (get :name)))

(defn same-group?
  [radio1 radio2]
  (= (radio-group radio1)
     (radio-group radio2)))

(defn broadcast-radio-checked
  [broadcaster root-element]
  (log/info "broadcast-radio-checked")
  (doseq [groupmate (->> root-element
                      focusable-element-locs
                      (map zip/node)
                      (filter (partial same-group? broadcaster))
                      (filter (partial not= broadcaster)))
          :let [set-value (-> groupmate second :gossamer.components/set-value)]
          :when set-value]
    (log/info "setting value for" groupmate)
    (set-value false)))

(defn advance-focus-index!
  [container]
  (swap! focus-index (fn [index]
    (let [num-focusable-elements (count (focusable-element-locs container))]
      (log/info "advance-focus-index!" index num-focusable-elements)
      (if (pos? num-focusable-elements)
        (if (= -1 index)
          0
          (mod (inc index) (count (focusable-element-locs container))))
        index)))))

(defn decrease-focus-index!
  [container]
  (swap! focus-index (fn [index]
    (mod (dec index) (count (focusable-element-locs container))))))

(defn active-element
  [container]
  (when-not (neg? @focus-index)
    (let [element-locs (focusable-element-locs container)]
      (when (< @focus-index (count element-locs))
        (-> element-locs
          (nth @focus-index)
          zip/node)))))

(defn handle-keypress
  [target-element root-element k action]
  (log/info "handle-keypress" (vec (take 2 target-element)))
  (when-let [on-keypress (-> target-element second :on-keypress)]
    (on-keypress {:key k :target target-element}))
  (when (= (-> target-element second :gossamer.components/type) :radio)
    (broadcast-radio-checked target-element root-element)))

(defn handle-focus
  [target-element]
  (log/info "setting focus on" target-element)
  (when-let [on-focus (-> target-element second :on-focus)]
    (on-focus {:target target-element})))

(defn handle-blur
  [target-element]
  (when-let [on-blur (-> target-element second :on-blur)]
    (on-blur {:target target-element})))

(defn initial-focus-index-loc
  [root-element]
  (->> root-element
    focusable-element-locs
    (map-indexed vector)
    (filter (fn [[_ element-loc]]
      (let [element (zip/node element-loc)
            props (second element)]
        (get props :autofocus))))
    first))

(defn render 
  ([terminal component props]
    (render terminal component props {})) 
  ([terminal component props style]
    ; render into an empty vector
    (let [term-pub (zt/pub terminal)
          keypress-chan (chan (buffer 100))
          click-chan (chan (buffer 100))
          mouse-enter-chan (chan (buffer 100))
          mouse-leave-chan (chan (buffer 100))
          exec-in-context (chan)
          last-render (atom nil)
          last-layout (atom nil)
          input-values (atom {})
          apply-in-context (atom nil)]
      (letfn [(on-render [container]
        (try
          (let [root-element (cm/clj-elements container)]
            #_(log/info "root-element" root-element)
            ; Render elements to chars, update terminal layers
            ; Root style map lifted here to encourage caching
            (let [root-style-map (merge-styles (style-map default-style) style)
                  dom (render-into-container terminal root-element root-style-map)
                  ; capture element layout
                  {:keys [layout-elements]} (meta dom)]
              ; uncomment to only draw one frame
              #_(System/exit 1)
              #_(log/info "layout-elements" (vec layout-elements))
              ; On first render set focusable index to first element claiming focus. -1 otherwise.
              (when (and (nil? @last-render)
                         @apply-in-context)
                #_(log/info "finding autofocus in" (mapv zip/node (zipper-descendants
                    (zipper-elements root-element))))
                ;; set initial focused element
                (when-let [[initial-focus-index initial-element-loc] (initial-focus-index-loc root-element)]
                  (log/info "setting focus-index" initial-focus-index)
                  (reset! focus-index initial-focus-index)
                  (@apply-in-context handle-focus (zip/node initial-element-loc))))
              ; Store last render for event handlers
              (reset! last-render root-element)
              ; Store last layout for event handlers
              #_(log/info "layout-elements")
              #_(doseq [elem layout-elements]
                (log/info "layout-element" elem))
              (reset! last-layout layout-elements)))
          (zt/refresh! terminal)
        (catch Throwable t
          (log/error t))))]
        (reset! apply-in-context
          (cm/render-with-context
            component
            props
            on-render))

      ; Subscribe to events and send them to chans
      (a/sub term-pub :keypress keypress-chan))
      (a/sub term-pub :click click-chan)
      (a/sub term-pub :mouse-enter mouse-enter-chan)
      (a/sub term-pub :mouse-leave mouse-leave-chan)

      (go-loop []
        (let [animation-frame-requests @request-animation-frame-handlers]
          ; Clear animation frame requests before processing existing requests
          ; because new animation frame requests may accumulate during the
          ; processing of existing requests.
          (reset! request-animation-frame-handlers [])
          (doseq [animation-frame-request animation-frame-requests]
            (try
              #_(log/info "Executing animation-frame-request" animation-frame-request)
              (@apply-in-context animation-frame-request)
              (catch Throwable t
                (log/error t)))))
        (recur))

       ;; All handlers use exec-in-context so that gossamer bindings are avalable during execution
       (go-loop []
         (when-let [layout @last-layout]
           (let [[event event-chan] (a/alts! [keypress-chan click-chan mouse-leave-chan mouse-leave-chan] :default nil)]
             (condp = event-chan
               :default nil
               mouse-enter-chan
                 (@apply-in-context handle-mouse-enter layout (first event))
               mouse-leave-chan
                 (@apply-in-context handle-mouse-leave layout (first event))
               keypress-chan
                 (let [[k action] event]
                   (case k
                     :tab
                       (do
                         (@apply-in-context handle-blur (active-element @last-render))
                         (advance-focus-index! @last-render)
                         (@apply-in-context handle-focus (active-element @last-render)))
                     ; else, send to active element
                     (@apply-in-context handle-keypress (active-element @last-render) @last-render k action)))
               click-chan
                 (@apply-in-context handle-click layout event))))
         ; Give the render a chance to render more than input poll
         (<! (a/timeout 1))
         (recur)))))

(defmacro defcomponent
  [compname args & body]
  `(cm/defcomponent ~compname ~args ~@body))
 
