(ns zaffre.components.render
  (:require clojure.data
            [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.layout :as zl]
            [zaffre.text :as ztext]
            [zaffre.terminal :as zt])
  (:import [java.text.AttributedString]))

(def default-style
  {:fg [255 255 255 255]
   :bg [0 0 0 255]
   :border-style :single
   :border 0
   :border-left 0
   :border-right 0
   :border-top 0
   :border-bottom 0
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
      [type (update-in props [:style] #(select-keys (merge parent-style %)
                                                    inheritable-styles)) children]
      (let [new-style (merge (select-keys parent-style inheritable-styles) (get props :style {}))]
        [type
         (assoc props :style new-style)
         (map (partial cascade-style new-style) children)]))))
      
; bounds are [x1 y1 x2 y2]
(defn layout->bounds [{:keys [x y width height]}]
  [x y (+ x width) (+ y height)])

(defn render-string-into-container [^"[[Ljava.lang.Object;" target x y fg bg s]
  {:pre [(number? x)
         (number? y)
         (vector? fg)
         (vector? bg)
         (string? s)]}
  (when (< y (count target))
    (let [^"[Ljava.lang.Object;" target-line (aget target y)
          max-x (count target-line)
          style #{}]
    (loop [index 0 s s]
      (let [target-index (+ x index)]
        (when (and (seq s) (< -1 target-index max-x))
          (aset target-line target-index {:c (first s) :fg fg :bg bg})
          (recur (inc index) (rest s))))))))

(defn render-text-into-container [target text-element]
  {:pre [(= (first text-element) :text)
         (map? (second text-element))
         (not (empty? (last text-element)))]}
  (log/debug "render-text-into-container" text-element)
  (let [[type {:keys [style zaffre/layout] :or {style {}} :as props} children] text-element
        {:keys [x y width height]} layout
        {:keys [text-align] :or {text-align :left}} style
        lines (ztext/word-wrap-text-tree width height (if (every? string? children)
                                                          [:text props (map (fn [child] [:text {} [child]]) children)]
                                                          text-element))]
    (log/trace "render-text-into-container lines" (vec lines))
    (doseq [[dy line] (map-indexed vector lines)]
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
          (doseq [[dx [s {:keys [fg bg]} _]] (map vector offsets line)]
            (render-string-into-container
              target
              (+ x dx) (+ y dy)
              (or fg (get default-style :fg))
              (or bg (get default-style :bg))
              (clojure.string/trim s))))))))

(defn render-img-into-container [target img-element]
  {:pre [(= (first img-element) :img)
         (map? (second img-element))
         #_(not (empty? (last img-element)))]}
  (log/trace "render-img-into-container" img-element)
  (let [[type {:keys [style zaffre/layout] :or {style {}} :as props} children] img-element
        {:keys [x y width height overflow-bounds]} layout
        overflow-bounds (or overflow-bounds (layout->bounds layout))
        [overflow-min-x overflow-min-y overflow-max-x overflow-max-y] overflow-bounds
        ;; bounds for overflow/target container (whichever is smaller)
        min-x (dec (max 1 overflow-min-x))
        min-y (dec (max 1 overflow-min-y))
        max-y (min overflow-max-y (count target))
        lines (last img-element)]
    (log/trace "render-img-into-container lines" (vec lines))
    (doseq [[dy line] (map-indexed vector lines)
            :let [target-y (+ y dy)]]
      (when (and (< dy height) (not (empty? line)) (< min-y target-y max-y))
        (log/trace "rendering line" (vec line))
        (doseq [[dx {:keys [c fg bg] :as pixel}] (map-indexed vector line)]
          (assert (char? c))
          (assert (vector? fg))
          (assert (vector? bg))
          (let [^"[Ljava.lang.Object;" target-line (aget target target-y)
                max-x (min overflow-max-x(count target-line))
                target-x (int (+ x dx))]
            (when (< min-x target-x max-x)
              (log/trace "rendering pixel x:" target-x " y:" target-y " " pixel " max-x:" max-x " max-y:" (count target))
              (aset target-line target-x pixel))))))))

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
          {:keys [fg bg
                  border border-style
                  border-top border-bottom
                  border-left border-right]} (merge default-style style)]
      ;; render background when set
      (when bg
        (doseq [dy (range height)
                :let [fg (or fg (get default-style :fg))]]
          (render-string-into-container target x (+ y dy) fg bg (apply str (repeat width " ")))))
      ; render border when set
      (when border-style
        (let [border-map (case border-style
                           :single single-border
                           :double double-border)]
          ; render top
          (when (or (< 0 border) (< 0 border-top))
            (render-string-into-container target x y fg bg
              (str (when (or (< 0 border) (< 0 border-left))
                     (get border-map :top-left))
                   (apply str (repeat (- width (+ (or border border-left) (or border border-right)))
                                      (get border-map :horizontal)))
                   (when (or (< 0 border) (< 0 border-right))
                     (get border-map :top-right)))))
          ; render middle
          (doseq [dy (range (- height 2))]
            (when (or (< 0 border) (< 0 border-left))
              (render-string-into-container target x (+ y dy 1) fg bg (str (get border-map :vertical))))
            (when (or (< 0 border) (< 0 border-right))
            (render-string-into-container target (+ x width -1) (+ y dy 1) fg bg (str (get border-map :vertical)))))
          ; render bottom
          (when (or (< 0 border) (< 0 border-bottom))
            (render-string-into-container target x (+ y height -1) fg bg
              (str (when (or (< 0 border) (< 0 border-left))
                     (get border-map :bottom-left))
                   (apply str (repeat (- width (+ (or border border-left) (or border border-right)))
                                (get border-map :horizontal)))
                   (when (or (< 0 border) (< 0 border-right))
                     (get border-map :bottom-right))))))))
    nil)

;; Do nothing for :layer
(defmethod render-component-into-container :layer
  [_ component]
  nil)

;; die if not :layer nor :view nor :text
(defmethod render-component-into-container :default
  [_ component]
  (assert false (format "Found unknown component %s" component)))

(defn intersect-bounds [[ax1 ay1 ax2 ay2 :as a-bounds] [bx1 by1 bx2 bt2 :as b-bounds]]
  (cond
    (nil? a-bounds)
      b-bounds
    (nil? b-bounds)
      a-bounds
    :else
      []))

(defn inherit-overflow-bounds
  ([element]
    (inherit-overflow-bounds nil element))
  ([parent-bounds element]
    (let [[type props children] element]
      (cond
        (or (= type :text) (= type :img))
          [type
           (assoc-in props [:zaffre/layout :overflow-bounds] parent-bounds)
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
    (log/trace "render-layer-into-container elements" elements)
    (doseq [element elements]
      (log/debug "render-layer-into-container element" element)
      (render-component-into-container target element))))
                    
(defn log-layer [^"[[Ljava.lang.Object;" layer-container]
  (doseq [y (range (alength layer-container))
          :let [^"[Ljava.lang.Object;" line (aget layer-container y)]]
    (doseq [x (range (alength line))
            :let [{:keys [c]} (aget line x)]]
      (if-not (nil? c)
        (print c)
        (print \u0000)))
    (println "")))
  
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
  (when element
    (zc/element-state-changed? zc/*updater* element)))

;; render a single element if needed
(defn render [existing parent element]
  (log/trace "existing" [(zc/element-display-name existing) (get existing :id)]
            "element"  [(zc/element-display-name element) (get element :id)])
  
  (log/trace "existing" (zc/tree->str existing))
  (log/trace "element" (zc/tree->str element))
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
    ;; identical to existing element. do nothing
    (and (type-match? existing element)
         (props-without-children-match? existing element)
         (not (state-changed? existing)))
      (do
         (log/trace "identical returning existing")
        (first (zc/element-children existing)))
    ;; same type, new props?
    (and (type-match? existing element)
         (or
           (not (props-without-children-match? existing element))
           (state-changed? existing)))
      ;; updating
      (binding [zc/*current-owner* existing]
        (let [_ (log/trace "same type, different-props or state" (zc/element-display-name existing))
              _ (log/trace "existing" (get existing :id) "element" (get element :id))
              ; copy prev state into instance because we're checking to see how the component
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
                 next-element (zc/render instance)
                 _ (zc/component-did-update instance prev-props prev-state)]
             (log/trace "should-component-update? = true" (get existing :id) (zc/element-display-name existing))
             (assoc next-element :id (get (first (zc/element-children existing)) :id)))
           (do
             (log/trace "should-component-update? = false" (get existing :id) (zc/element-display-name existing))
             (first (zc/element-children existing))))))
    ;; initial render
    :default
    (binding [zc/*current-owner* element]
      (let [_ (log/trace "initial-render" "existing" (zc/element-display-name existing) "element" (zc/element-display-name element))
            instance (zc/construct-instance element)
            _ (log/trace "constructed component")
            ;; derive state from props
            derived-state (let [next-props (get element :props)
                                prev-state (zc/get-state zc/*updater* element)]
                            (zc/get-derived-state-from-props instance next-props prev-state))
            _ (when derived-state
                (zc/enqueue-set-state! zc/*updater* element derived-state nil))
            _ (zc/component-will-mount instance)
            _ (log/trace "rendering component" (str instance))
            next-element (zc/render instance)
            _ (zc/component-did-mount instance)]
        ;; unmount existing if it exists
        (if (and existing (zc/component? (get existing :type)))
          (do
            (log/trace "unmounting" (get existing :id) (zc/element-display-name existing))
            (zc/component-will-unmount (zc/construct-instance existing))
            (zc/enqueue-remove-state! zc/*updater* existing))
          (log/error "could not unmount" (get existing :type)))
        next-element))))

(defn- display-name [element-type]
  (cond
    (nil? element-type)
      "Nil"
    (keyword? element-type)
      (name element-type)
    :else
      (zc/display-name element-type)))

(defn- without-type [v]
  (if (map? v)
    (dissoc v :type)
    v))

(defn first-element-id [existing element]
  (get existing :id (get element :id)))

(defn render-recursively 
  ([element]
    (render-recursively nil element))
  ([existing element]
    (render-recursively existing nil element))
  ([existing parent element]
    (log/trace "render-recursively existing" (zc/element-display-name existing) (count (zc/element-children existing)) #_(without-type existing))
    (log/trace "render-recursively element" (zc/element-display-name element) (count (zc/element-children element)) #_(without-type element))
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
          element
          ;; render other built-ins by rendering their chldren
          (let [rendered-children (zc/map-children (fn [child existing-child]
                                                     (render-recursively existing-child element child))
                                                   element
                                                   existing)]
            (zc/assoc-children element rendered-children)))
      :default
        (let [;; Render one level
              rendered-element (render existing parent element)]
          (log/trace "rendered element" (str (get rendered-element :type)))
          (if false #_existing
            (zc/assoc-children existing (zc/map-children (fn [child existing-child]
                                                           (render-recursively existing-child element child))
                                                          rendered-element
                                                          existing))
            ; take the rendered element and assign it as the child to `element`.
            ; render recursively, the rendered-element 
            (-> element
              (zc/assoc-children [(render-recursively (first (zc/element-children existing))
                                                      element
                                                      rendered-element)])
              ;; assign existing id so that state lookup works
              (assoc :id (first-element-id existing element))))))))

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

;; Renders component into container. Does not call refresh! on container
(defn render-into-container
  ([target component]
    (render-into-container target nil component))
  ([target existing component]
    (let [group-info (zt/groups target)
          layer-info (layer-info group-info)
          ;; render to native elements
          root-element (render-recursively existing component)
          [type props groups :as root-dom] (-> root-element
                                              extract-native-elements
                                              first
                                              cascade-style)]
      (log/trace "render-into-container existing" (zc/tree->str existing))
      (log/trace "render-into-container element" (zc/tree->str root-element))
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
          (let [layer-container (object-array rows)
                ;; merge layer width height into layer's style
                {:keys [columns rows]} (get layer-info id)
                style (merge style {:width columns :height rows})]
            (doseq [i (range rows)]
              (aset layer-container i (object-array columns)))
            (log/trace "render-into-container - layer" id)
            ;; render layer into layer-container
            (render-layer-into-container
               layer-container
               (-> layer
                 (assoc-in [1 :style :width] columns)
                 (assoc-in [1 :style :height] rows)))
            #_(log-layer layer-container)
            ;; replace chars in layer
            (zt/replace-chars! target id layer-container))))
      root-element)))

