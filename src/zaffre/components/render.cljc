(ns zaffre.components.render
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.layout :as zl]
            [zaffre.text :as ztext]
            [zaffre.terminal :as zt])
  (:import [java.text.AttributedString]))

(def default-style
  {:fg [255 255 255 255]
   :bg [0 0 0 255]
})

;; Primitive elements
(def primitive-elements #{:terminal :group :layer :view :text})
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

(defn cascade-style
  ([element] (cascade-style {} element))
  ([parent-style [type props children]]
    (if (= type :text)
      [type (update-in props [:style] #(apply dissoc (merge parent-style %) text-forbidden-styles)) children]
      (let [new-style (merge parent-style (get props :style {}))]
        [type
         (assoc props :style new-style)
         (map (partial cascade-style new-style) children)]))))
      

(defn render-string-into-container [^"[[Ljava.lang.Object;" target x y fg bg s]
  {:pre [(number? x)
         (number? y)
         (vector? fg)
         (vector? bg)
         (string? s)]}
  (when (< y (count target))
    (let [^"[Ljava.lang.Object;" target-line (aget target y)
          max-x (count target-line)]
    (loop [index 0 s s]
      (let [target-index (+ x index)]
        (when (and (seq s) (< target-index max-x))
          (aset target-line target-index {:c (first s) :fg fg :bg bg})
          (recur (inc index) (rest s))))))))

(defn render-text-into-container [target text-element]
  {:pre [(= (first text-element) :text)
         (map? (second text-element))
         (not (empty?  (last text-element)))]}
  (log/trace "render-text-into-container" text-element)
  (let [[type {:keys [style zaffre/layout] :or {style {}} :as props} children] text-element
        {:keys [x y width height]} layout
        {:keys [text-align] :or {text-align :left}} style
        lines (ztext/word-wrap-text-tree width height text-element)]
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


(defn element-seq [element]
  "Returns :view and top-level :text elements."
  (tree-seq (fn [[type _ _]] (not= type :text))
            (fn [[_ _ children]] children)
            element))

(defmulti render-component-into-container (fn [_ element] (first element)))

;; render text
(defmethod render-component-into-container :text
  [target text-element]
  (render-text-into-container target text-element))

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
          {:keys [fg bg border-style]} style]
      ;; render background when set
      (when bg
        (doseq [dy (range height)
                :let [fg (or fg (get default-style :fg))]]
          (render-string-into-container target x (+ y dy) fg bg (repeat width " "))))
      ; render border when set
      (when border-style
        (let [{:keys [fg bg border-style]} (merge default-style style)
              border (case border-style
                        :single single-border
                        :double double-border)]
          ; render top
          (render-string-into-container target x y fg bg
            (str (get border :top-left) (apply str (repeat (- width 2) (get border :horizontal))) (get border :top-right)))
          ; render middle
          (doseq [dy (range (- height 2))]
            (render-string-into-container target x (+ y dy 1) fg bg (str (get border :vertical)))
            (render-string-into-container target (+ x width -1) (+ y dy 1) fg bg (str (get border :vertical))))
          ; render bottom
          (render-string-into-container target x (+ y height -1) fg bg
            (str (get border :bottom-left) (apply str (repeat (- width 2) (get border :horizontal))) (get border :bottom-right))))))
    nil)

;; Do nothing for :layer
(defmethod render-component-into-container :layer
  [_ component]
  nil)

;; die if not :layer nor :view nor :text
(defmethod render-component-into-container :default
  [_ component]
  (assert false (format "Found unknown component %s" component)))

(defn render-layer-into-container
  [target layer]
  (log/trace "render-layer-into-container" layer)
  (let [layer-en-place (zl/layout-element layer)
        elements (vec (element-seq layer-en-place))]
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
(defn construct-instance [element]
  {:pre [(zc/element? element)]
   :post [(zc/component-instance? %)]}
  (log/trace "constructing component instance" (str element))
  (let [{:keys [type props]} element
        state (or (zc/get-state zc/*updater* element)
                  (when (zc/component? type)
                    (let [s ((get type :get-initial-state))]
                      (-> zc/*updater*
                        (zc/enqueue-set-state! element s nil)
                        zc/update-state!
                        (zc/get-state element)))))
        _ (log/trace "construct-component element type" (str type))
        instance (if (fn? type)
                   (zc/create-instance (assoc (zc/fn->component type) :props props))
                   (zc/create-instance (assoc type :props props) state))]
    instance))

(defn type-match? [existing element]
   (= (get element :type)
      (get existing :type)))

(defn props-without-children-match? [existing element]
   (= (get (zc/element-without-children element) :props)
      (get (zc/element-without-children existing) :props)))

(defn state-changed? [element]
   (not (zc/element-state-changed? zc/*updater* element)))

;; render a single element if needed
(defn render [existing parent element]
  (if (type-match? existing element)
    (log/trace "type-match? true")
    (log/trace "type-mismatch" (get existing :type) (get element :type)))
  (if (props-without-children-match? existing element)
   (log/trace "props-without-children-match? true")
   (log/trace "props-without-children-mismatch" (get (zc/element-without-children existing) :props)
                                               (get (zc/element-without-children element) :props)))
  (if (state-changed? element)
    (log/trace "state-changed" (zc/get-state zc/*updater* existing) (zc/get-state zc/*updater* element))
    (log/trace "state-changed? false"))
  ;; if element = existing, no-op return element
  (cond
    ;; identical to existing element. do nothing
    (and (type-match? existing element)
         (props-without-children-match? existing element)
         (not (state-changed? element)))
      (do (log/trace "identical returning existing")
      (get-in existing [:props :children 0]))
    ;; same type, new props?
    (and (type-match? existing element)
         (or
           (not (props-without-children-match? existing element))
           (state-changed? element)))
      ;; updating
      (let [_ (log/trace "same type, different-props")
            instance (construct-instance element)
            prev-props (get existing :props)
            next-props (get element :props)
            prev-state (zc/get-prev-state zc/*updater* element)
            next-state (zc/get-state zc/*updater* element)
            ;; only call when props have changed
            _ (when-not (props-without-children-match? existing element)
                (zc/component-will-receive-props instance next-props))]
       (if (zc/should-component-update? instance prev-props next-props)
         (let [_ (zc/component-will-update instance next-props next-state)
               next-element (zc/render instance)
               _ (zc/component-did-update instance prev-props prev-state)]
           next-element)
         element))
    ;; initial render
    :default
    (let [_ (log/trace "initial-render")
          instance (construct-instance element)
          _ (log/trace "constructed component")
          ;; derive state from props
          derived-state (let [next-props (get element :props)
                              prev-state (zc/get-state zc/*updater* element)]
                          (zc/get-derived-state-from-props instance next-props prev-state))
          _ (-> zc/*updater*
              (zc/enqueue-set-state! element derived-state nil)
              zc/update-state!
              (zc/get-state element))
          _ (zc/component-will-mount instance)
          _ (log/trace "rendering component" (str instance))
          next-element (zc/render instance)
          _ (zc/component-did-mount instance)]
      next-element)))

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

(defn render-recursively 
  ([element]
    (render-recursively nil element))
  ([existing element]
    (render-recursively existing nil element))
  ([existing parent element]
    (log/trace "render-recursively existing" (display-name (get existing :type)) #_(without-type existing))
    (log/trace "render-recursively element" (display-name (get element :type)) #_(without-type element))
    (cond
      (nil? element)
        element
      (string? element)
        (do (log/trace "rendering string" element)
        element)
      ;; built-in?
      (keyword? (get element :type))
        (let [rendered-children (zc/map-children (fn [child existing-child]
                                                   (render-recursively existing-child element child))
                                                 element
                                                 existing)]
          (if (and existing
                ;; identical to existing element. do nothing
                (type-match? existing element)
                (props-without-children-match? existing element)
                (not (state-changed? element)))
            (zc/assoc-children existing rendered-children)
            (zc/assoc-children element rendered-children)))
      :default
        (let [;; Render one level
              rendered-element (render existing parent element)]
          (log/trace "rendered element" (str (get rendered-element :type)))
          (if false #_existing
            (zc/assoc-children rendered-element (zc/map-children (fn [child existing-child]
                                                                   (render-recursively existing-child element child))
                                                                 rendered-element
                                                                 existing))
            ; take the rendered element and assign it as the child to `element`.
            ; render recursively, the rendered-element 
            (zc/assoc-children element [(render-recursively existing
                                                            element
                                                            rendered-element)]))))))

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
      (log/trace "render-into-container" root-dom)
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
                  (format "Expected :layer found %s instead" type))
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

