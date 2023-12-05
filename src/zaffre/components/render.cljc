(ns zaffre.components.render
  (:require clojure.data
            [clojure.string]
            [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.layout :as zl]
            [zaffre.text :as ztext]
            [zaffre.terminal :as zt]
            [zaffre.util :as zu]))

(def default-style
  {:color (unchecked-int 0xffffffff) #_[255 255 255 255]
   :background-color (unchecked-int 0x000000ff) #_[0 0 0 255]
   :border-style :single
   :border 0
   :border-left 0
   :border-right 0
   :border-top 0
   :border-bottom 0
   :mix-blend-mode :normal
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
  :word-spacing})

(defn cascade-style
  ([element] (cascade-style {} element))
  ([parent-style [type props children]]
    (if (or (= type :text) (= type :img))
      [type
       (update-in props [:style] #(merge (select-keys parent-style inheritable-styles) %))
       children]
      (let [new-style (merge (select-keys parent-style inheritable-styles) (get props :style {}))]
        [type
         (assoc props :style new-style)
         (map (partial cascade-style new-style) children)]))))
      
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
            (zt/set! target c blend-mode color background-color col row)
            (recur (inc index) (rest s))))))))

(defn render-text-into-container [target text-element]
  {:pre [(= (first text-element) :text)
         (map? (second text-element))
         (not (empty? (last text-element)))]}
  (log/debug "render-text-into-container" text-element)
  (let [[type {:keys [style zaffre/layout] :or {style {}} :as props} children] text-element
        {:keys [x y width height]} layout
        {:keys [text-align mix-blend-mode] :or {text-align :left mix-blend-mode :normal}} style
        lines (ztext/word-wrap-text-tree width height (if (every? string? children)
                                                          [:text props (map (fn [child] [:text {} [child]]) children)]
                                                          text-element))]
    (log/trace "render-text-into-container lines" (vec lines))
    (zu/loop-with-index dy [line lines]
      (let [default-color (get default-style :color)
            default-background-color (get default-style :background-color)]
        (when (and (< dy height) (not (empty? line)))
          (log/trace "rendering line" (vec line))
          ;; remove inc? for spaces
          (let [but-last (butlast line)
                last-span (let [[s style _] (last line)
                                s (clojure.string/trim s)]
                           [s style (count s)])
                line (concat but-last (list last-span))
                _ (log/trace "rendering line2" (vec line))
                span-lengths (map (fn [[s _ length]] length) line)
                line-length (reduce + 0 span-lengths)
                align-offset (case text-align
                               :left 0
                               :center (/ (- width line-length) 2)
                               :right (- width line-length))
                offsets (map (partial + align-offset) (cons 0 (reductions + span-lengths)))]
            (log/trace "line-length" line-length)
            (log/trace "lengths" (vec span-lengths))
            (log/trace "offsets" (vec offsets))
            (doseq [[dx [s {:keys [color background-color]} _]] (map vector offsets line)]
              (render-string-into-container
                target
                (+ x dx) (+ y dy)
                (or color default-color)
                (or background-color default-background-color)
                (clojure.string/trim s)
                mix-blend-mode))))))))

(defn render-img-into-container [target img-element]
  {:pre [(= (first img-element) :img)
         (map? (second img-element))
         #_(not (empty? (last img-element)))]}
  #_(log/debug "render-img-into-container" img-element)
  (let [[type {:keys [style zaffre/layout] :or {style {}} :as props} children] img-element
        {:keys [mix-blend-mode] :or {mix-blend-mode (get default-style :mix-blend-mode)}} style
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
        lines (last img-element)]
    #_(log/debug "render-img-into-container layout " layout)
    #_(log/trace "render-img-into-container lines" (vec lines))
    (zu/loop-with-index dy [line lines]
      (let [y (int y)
            dy (int dy)
            row (+ y dy)]
        (when (and (< dy height) (not (empty? line)) (< min-y row max-y))
          #_(log/trace "rendering line" (vec line))
          (zu/loop-with-index dx [{:keys [c fg bg] :as pixel} line]
            (assert (char? c))
            (assert (or (integer? fg) (vector? fg)))
            (assert (or (integer? bg) (vector? bg)))
            (let [max-x (min overflow-max-x num-cols-target)
                  x (int x)
                  dx (int dx)
                  col (int (+ x dx))]
              (when (< min-x col max-x)
                #_(log/trace "rendering pixel x:" col " y:" row " " pixel " max-x:" max-x " max-y:" (zt/num-rows target))
                (zt/set! target (get pixel :c) mix-blend-mode (get pixel :fg) (get pixel :bg) col row)))))))))

(defn element-seq [element]
  "Returns :view and top-level :text elements."
  (tree-seq (fn [[type _ _]] (and (not= type :text) (not= type :img)))
            (fn [[_ _ children]] children)
            element))

(defmulti render-component-into-container (fn [_ element] (first element)))

;; render text
(defmethod render-component-into-container :text
  [target text-element]
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
  [target [type {:keys [style zaffre/layout]}]]
    (let [{:keys [x y width height]} layout
          {:keys [color background-color
                  border border-style
                  border-top border-bottom
                  border-left border-right
                  mix-blend-mode]} (merge default-style style)]
      ;; render background when set
      (when background-color
        (doseq [dy (range height)
                :let [color (or color (get default-style :color))]]
          (render-string-into-container target x (+ y dy) color background-color (clojure.string/join (repeat width " ")) mix-blend-mode)))
      ; render border when set
      (when border-style
        (let [border-map (case border-style
                           :single single-border
                           :double double-border)]
          ; render top
          (when (or (< 0 border) (< 0 border-top))
            (render-string-into-container target x y color background-color
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
              (render-string-into-container target x (+ y dy 1) color background-color (str (get border-map :vertical)) mix-blend-mode))
            (when (or (< 0 border) (< 0 border-right))
              (render-string-into-container target (+ x width -1) (+ y dy 1) color background-color (str (get border-map :vertical)) mix-blend-mode)))
          ; render bottom
          (when (or (< 0 border) (< 0 border-bottom))
            (render-string-into-container target x (+ y height -1) color background-color
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

(defn inherit-overflow-bounds
  ([element]
    (inherit-overflow-bounds nil element))
  ([parent-bounds element]
    (let [[type props children] element]
      (cond
        (or (= type :text) (= type :img))
          [type
           (if parent-bounds
             (assoc-in props [:zaffre/layout :overflow-bounds] parent-bounds)
             props)
           children]
        (= (get-in props [:style :overflow]) :hidden)
          [type
           props
           (map (partial inherit-overflow-bounds (layout->bounds (get props :zaffre/layout))) children)]
        :else
          [type
           props
           (map (partial inherit-overflow-bounds parent-bounds) children)]))))

(defn render-layer-into-container
  [target layer]
  (log/trace "render-layer-into-container" layer)
  (let [layer-en-place (zl/layout-element layer)
        elements (-> (inherit-overflow-bounds layer-en-place)
                   element-seq
                   vec)]
    #_(log/trace "render-layer-into-container elements" elements)
    #_(log/trace "render-layer-into-container layer-en-place" (zc/tree->str layer-en-place))
    (doseq [element elements]
      #_(log/debug "render-layer-into-container element" element)
      (render-component-into-container target element))
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
(defn type-match? [existing element]
   (= (get element :type)
      (get existing :type)))

(defn props-without-children-match? [existing element]
   (= (get (zc/element-without-children element) :props)
      (get (zc/element-without-children existing) :props)))

(defn state-changed? [element]
  (when (and element (get element :id))
    (zc/element-state-changed? zc/*updater* element)))

(defn- check-elem [existing element result]
 true
 #_(if(zc/element? result)
    true
    (do
      (log/error "render did not returnn element")
      (log/error "existing")
      (log/error (zc/tree->str existing))
      (log/error "element")
      (log/error (zc/tree->str element))
      (log/error "result")
      (log/error (zc/tree->str result))
      false)))

(defn first-element-id [& elements]
  (some :id elements))

;; render a single element if needed
(defn render [existing parent element]
  #_{:pre [(is (or (nil? existing) (zc/element? existing)) (zc/tree->str element))
         (is (zc/element? element) (zc/tree->str element))]
   :post [(is (or (nil? %) (string? %) (and (zc/element? %) (check-elem existing element %))) (zc/element-display-name %))]}
  (log/trace "existing" [(zc/element-display-name existing) (zc/element-id-str existing)]
            "element"  [(zc/element-display-name element) (zc/element-id-str element)])
  
  #_(log/info "existing" (zc/tree->str existing))
  #_(log/info "element" (zc/tree->str element))
  (if (type-match? existing element)
    (log/trace "type-match? true")
    (log/trace "type-mismatch" (get existing :type) (get element :type)))
  (if (and (zc/element? existing)
           (props-without-children-match? existing element))
   (log/trace "props-without-children-match? true")
   (log/trace "props-without-children-mismatch" (vec (take 2
                                                  (clojure.data/diff
                                                    (get (zc/element-without-children existing) :props)
                                                    (get (zc/element-without-children element) :props))))))
  (if (and (zc/element? existing)
           (state-changed? existing))
    (log/trace "state-changed" (zc/get-state zc/*updater* existing))
    (log/trace "state-changed? false"))
  ;; if element = existing, no-op return element
  (cond
    ;; identical to existing element. skip lifecycle, just render
    (and (type-match? existing element)
         (props-without-children-match? existing element)
         (not (state-changed? existing)))
      (binding [zc/*current-owner* existing]
        #_(log/info "identical rendering element" (zc/element-display-name existing))
        #_(log/info "existing" (zc/element-id-str existing) "element" (zc/element-id-str element))
        #_(log/info "element children:\n" (first (zc/element-children element)))
        (let [instance (zc/construct-instance (assoc element :id (get existing :id)))
              next-element (zc/render instance)]
          next-element))
    ;; same type, new props?
    (and (type-match? existing element)
         (or
           (not (props-without-children-match? existing element))
           (state-changed? existing)))
      ;; updating
      (binding [zc/*current-owner* existing]
        #_(log/info "same type, different-props or state" (zc/element-display-name existing))
        #_(log/info "existing" (zc/element-id-str existing) "element" (zc/element-id-str element))
        (let [; copy prev state into instance because we're checking to see how the component
              ; handles the change from prev-state to next state
              instance (zc/construct-instance existing)
              prev-instance (assoc instance :state (zc/get-prev-state zc/*updater* existing))
              prev-props (get existing :props)
              next-props (get element :props)
              prev-state (zc/get-prev-state zc/*updater* existing)
              next-state (zc/get-state zc/*updater* existing)
              ;; only call when props have changed
              _ (when-not (props-without-children-match? existing element)
                  (zc/component-will-receive-props prev-instance next-props))]
         (if (zc/should-component-update? prev-instance next-props next-state)
           (let [_ (zc/component-will-update prev-instance next-props next-state)
                 instance (zc/construct-instance (assoc element :id (get existing :id)))
                 next-element (zc/render instance)
                 _ (zc/component-did-update instance prev-props prev-state)]
             (log/debug "should-component-update? = true" (get existing :id) (zc/element-display-name existing))
             (log/trace "next-element" (type next-element))
             (log/trace (zc/element-display-name next-element))
             #_(log/trace (zc/tree->str next-element))
             next-element)
           (do
             (assert false)
             (log/info "should-component-update? = false" (get existing :id) (zc/element-display-name existing))
             (first (zc/element-children existing))))))
    ;; initial render
    :default
    (binding [zc/*current-owner* element]
      #_(log/info "initial-render" (type element))
      #_(log/info "initial-render" (type existing))
      #_(log/info "initial-render" "existing" (zc/element-display-name existing) "element" (zc/element-display-name element))
      (let [instance (zc/construct-instance element)
            _ (log/trace "constructed component")
            ;; derive state from props
            derived-state (let [next-props (get element :props)
                                prev-state (zc/get-state zc/*updater* element)]
                            (zc/get-derived-state-from-props instance next-props prev-state))
            _ (when derived-state
                (zc/immediate-set-state! zc/*updater* element derived-state))
            _ (zc/component-will-mount instance)
            _ (log/trace "rendering component" (str instance))
            next-element (zc/render instance)
            _ (zc/component-did-mount instance)]
        ;; unmount existing if it exists
        (when (and existing (zc/component? (get existing :type)))
            (log/trace "unmounting" (get existing :id) (zc/element-display-name existing))
            (zc/component-will-unmount (zc/construct-instance existing))
            (zc/enqueue-remove-state! zc/*updater* existing))
        next-element))))

(defn- without-type [v]
  (if (map? v)
    (dissoc v :type)
    v))

(defn render-recursively 
  ([element]
    (render-recursively nil element))
  ([existing element]
    (render-recursively existing nil element))
  ([existing parent element]
   {:post [(is (or (nil? %) (string? %) (and (zc/element? %) (check-elem existing element %))) (zc/element-display-name %))]}
    #_(when (= existing element)
      (log/warn "WARNING Existing = Element WARNING"))
    (log/trace "render-recursively existing" (zc/element-display-name existing) (count (zc/element-children existing)) #_(without-type existing))
    (log/trace "render-recursively element" (zc/element-display-name element) (count (zc/element-children element)) #_(without-type element))
    #_(log/trace "existing:\n" (zc/tree->str existing))
    #_(log/trace "element:\n" (zc/tree->str element))
    (cond
      (nil? element)
        element
      (string? element)
        (do (log/trace "rendering string" element)
        element)
      ;; built-in?
      (keyword? (get element :type))
        (if (= (get element :type) :img)
          ;; imgs have pre-rendered pixel children
          (assoc element :id (get existing :id))
          ;; render other built-ins by rendering their chldren
          (let [rendered-children (zc/map-children (fn [child existing-child]
                                                     (render-recursively existing-child element child))
                                                   element
                                                   existing)]
            (-> element
              (zc/assoc-children (remove nil? rendered-children))
              (assoc
                :id
                (if (type-match? existing element)
                  (first-element-id existing element)
                  (get element :id))))))
      :default
        (let [;; Render one level
              rendered-element (render (when (zc/element? existing) existing)
                                       parent
                                       element)
              rendered-child (render-recursively (first (zc/element-children existing))
                                                    element
                                                    rendered-element)]
          (log/trace "rendered element" (zc/element-display-name rendered-element))
          (log/trace "element-id" (zc/element-id-str rendered-child))
          (assert (is (zc/element? rendered-element)))
          (assert (zc/element? rendered-child)
            (str (zc/element-display-name element) " -> "
                 (zc/element-display-name rendered-element) " -> "
                 (zc/element-display-name rendered-child) "\n"
                 rendered-child))
          ; take the rendered element and assign it as the child to `element`.
          ; render recursively, the rendered-element 
          (-> element
            (zc/assoc-children [rendered-child])
            ;; assign existing id so that state lookup works
            ;; FIXME find out why this kills Popup
            (assoc
              :id
              (if (type-match? existing element)
                (first-element-id existing element)
                (get element :id))))))))

(defn extract-native-elements [element]
  (cond
    (nil? element)
      []
    (= :text (get element :type))
      [[:text
       (dissoc (get element :props {}) :children)
       (let [children (zc/element-children element)]
         (if (every? string? children)
           children
           (mapcat extract-native-elements children)))]]
    (= :img (get element :type))
      [[:img
       (dissoc (get element :props {}) :children)
       (zc/element-children element)]]
    (keyword? (get element :type))
      [[(get element :type)
       (dissoc (get element :props {}) :children)
       (vec (mapcat extract-native-elements (zc/element-children element)))]]
    :default
      (vec (mapcat extract-native-elements (zc/element-children element)))))

(def terminal-layer-buffers (atom {}))

;; Renders component into container. Does not call refresh! on container
(defn render-into-container
  "Renders element into target. If existing is supplied, appropriate component
   lifecycle handlers will be invoked. Response is rendered elements with a
   meta property :layout-elements containing component id and layout records."
  ([target element]
    (render-into-container target nil element))
  ([target existing element]
    (let [group-info (zt/groups target)
          layer-info (layer-info group-info)
          ;; render to native elements
          root-element (render-recursively existing element)
          [type props groups :as root-dom] (-> root-element
                                              extract-native-elements
                                              first
                                              cascade-style)
          elements (atom (list))]
      #_(log/info "render-into-container existing" (zc/tree->str existing))
      #_(log/info "render-into-container rendered-element" (zc/tree->str root-element))
      ;(log/trace "render-into-container" (clojure.pprint/pprint root-dom))
      (assert (= type :terminal)
              (format "Root component not :terminal found %s instead" type))
      ;; for each group in terminal
      (doseq [[type {:keys [id pos]} layers] groups
              :let [{:keys [columns rows]} (get group-info id)]]
        (assert (= type :group)
                (format "Expected :group found %s instead" type))
        (log/trace "rendering group" id)
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
                                #_(or (get @terminal-layer-buffers id)
                                    (let [buffers (zt/array-buffers columns rows)]
                                      (swap! terminal-layer-buffers assoc id buffers)
                                      buffers))
                ;; merge layer width height into layer's style
                {:keys [columns rows]} (get layer-info id)]
            (log/trace "render-into-container - layer" id)
            ;; reusing layer-container so zero out before rendering
            ;(zt/zero! layer-container)
            ;; render layer into layer-container
            (swap! elements concat
              (render-layer-into-container
                 layer-container
                 (-> layer
                   ;; assign width and height to layer style
                   (assoc-in [1 :style :width] columns)
                   (assoc-in [1 :style :height] rows))))
            #_(log-layer id layer-container)
            ;; send layer changes to terminal
            (zt/put-layer! target id layer-container))))
      (with-meta root-element {:layout-elements @elements}))))

