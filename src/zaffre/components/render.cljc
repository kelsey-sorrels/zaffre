(ns zaffre.components.render
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.terminal :as zt]))


;; Primitive elements
(def primitive-elements #{:terminal :group :layer :view :text})
(defn primitive? [component]
  (or (string? component)
      (contains? primitive-elements (first component))))

(defn flatten-text [parent-style element]
  "Splits text into words and flattens all child text element"
  (lazy-seq
    (if (string? element)
      (let [words (clojure.string/split (clojure.string/trim element) #"\s+")]
        (interpose
          [:text (assoc parent-style :zaffre/children [" "])]
          (map (fn [word] [:text (assoc parent-style :zaffre/children [word])])
            words)))
      (let [[type {:keys [style zaffre/children]}] element]
        (if (= type :text)
          (mapcat (partial flatten-text (merge parent-style style)) children)
          (assert false (format "found non-text element %s" type)))))))

(defn text-length [text-element]
  (-> text-element second :zaffre/children first count))

(defn space? [text-element]
  (= " " (-> text-element second :zaffre/children first)))

;; Adapted from https://www.rosettacode.org/wiki/Word_wrap#Clojure
(defn wrap-lines [size style text]
  "Text elements may only contain child text elements"
  (loop [left size line [] lines []
         words (or (flatten-text style text)
                   [[:text {:zaffre/children [""]}]])]
    (log/trace "wrap-lines words" left style (vec words))
    (if-let [word (first words)]
      (let [wlen (text-length word)
            spacing (if (== left size) "" " ")
            alen (+ (count spacing) wlen)]
        (log/trace "wlen" wlen "spacing" spacing "alen" alen)
        (if (<= alen left)
          (recur (- left alen) (conj line word) lines (next words))
          (if (space? word)
            (recur size [] (conj lines line) (next words))
            (recur (- size wlen) [word] (conj lines line) (next words)))))
      (if (seq line)
        (conj lines line)
        words))))

(defn render-string-into-container [target x y fg bg s]
  (when (< y (count target))
    (let [target-line (aget target y)
          max-x (count target-line)]
    (log/trace "render-string-into-container" x y fg bg s)
    (loop [index 0 s s]
      (let [target-index (+ x index)]
        (when (and (seq s) (< target-index max-x))
          (aset target-line target-index {:c (first s) :fg fg :bg bg})
          (recur (inc index) (rest s))))))))

(def default-style
  {:fg [255 255 255 255]
   :bg [0 0 0 255]
})

(defn render-text-into-container [target text]
  (log/debug "render-text-into-container" text)
  (let [[type {:keys [zaffre/style zaffre/layout zaffre/children] :as props}] text
        {:keys [x y width height]} layout
        style (merge default-style style)
        lines (wrap-lines width
                          style
                          text)]
    (log/debug "render-text-into-container lines" lines)
    (doseq [[dy line] (map-indexed vector lines)]
      ;; remove inc? for spaces
      (let [offsets (cons 0 (reductions + (map (fn [word] (inc (count (last word)))) line)))]
        (doseq [[dx {:keys [fg bg text]}] (map vector offsets line)]
          (when (< dy height)
            (render-string-into-container target (+ x dx) (+ y dy) fg bg text)))))))


(defn component-seq [component]
  (tree-seq (fn [[_ {:keys [zaffre/children]}]] (every? (comp not string?) children))
            (fn [[_ {:keys [zaffre/children]}]] children)
            component))

(defmulti render-component-into-container (fn [target [type]] type))

;; render text
(defmethod render-component-into-container :text
  [target [type {:keys [zaffre/children zaffre/style zaffre/layout]}]]
  (doseq [child children]
    (render-text-into-container target child)))

;; render views
(defmethod render-component-into-container :view
  [target [type {:keys [zaffre/style zaffre/layout]}]]
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
  (log/debug "render-layer-into-container" layer)
  (let [descendants (vec (component-seq layer))]
    (log/debug "render-layer-into-container descendants" descendants))
  (doseq [descendant (component-seq layer)]
    (render-component-into-container target descendant)))
                    
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
  (log/debug "constructing component instance" (str element))
  (let [{:keys [type props]} element
        state (or (zc/get-state zc/*updater* element)
                  (when (zc/component? type)
                    (let [s ((get type :get-initial-state))]
                      (-> zc/*updater*
                        (zc/enqueue-set-state! element s nil)
                        zc/update-state!
                        (zc/get-state element)))))
        _ (log/debug "construct-component element type" (str type))
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
    (log/debug "type-match? true")
    (log/debug "type-mismatch" (get existing :type) (get element :type)))
  (if (props-without-children-match? existing element)
   (log/debug "props-without-children-match? true")
   (log/debug "props-without-children-mismatch" (get (zc/element-without-children existing) :props)
                                               (get (zc/element-without-children element) :props)))
  (if (state-changed? element)
    (log/debug "state-changed" (zc/get-state zc/*updater* existing) (zc/get-state zc/*updater* element))
    (log/debug "state-changed? false"))
  ;; if element = existing, no-op return element
  (cond
    ;; identical to existing element. do nothing
    (and (type-match? existing element)
         (props-without-children-match? existing element)
         (not (state-changed? element)))
      (do (log/debug "identical returning existing")
      (get-in existing [:props :children 0]))
    ;; same type, new props?
    (and (type-match? existing element)
         (or
           (not (props-without-children-match? existing element))
           (state-changed? element)))
      ;; updating
      (let [_ (log/debug "same type, different-props")
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
    (let [_ (log/debug "initial-render")
          instance (construct-instance element)
          _ (log/debug "constructed component")
          ;; derive state from props
          derived-state (let [next-props (get element :props)
                              prev-state (zc/get-state zc/*updater* element)]
                          (zc/get-derived-state-from-props instance next-props prev-state))
          _ (-> zc/*updater*
              (zc/enqueue-set-state! element derived-state nil)
              zc/update-state!
              (zc/get-state element))
          _ (zc/component-will-mount instance)
          _ (log/debug "rendering component" (str instance))
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
    (log/debug "render-recursively existing" (display-name (get existing :type)) #_(without-type existing))
    (log/debug "render-recursively element" (display-name (get element :type)) #_(without-type element))
    (cond
      (nil? element)
        element
      (string? element)
        (do (log/debug "rendering string")
        element)
      ;; built-in?
      (keyword? (get element :type))
        (let [_ (log/debug "rendering element")
              _ (log/debug "rendered element" (get element :type))
              rendered-children (zc/map-children (fn [child existing-child]
                                                   (log/debug "mapped child" child)
                                                   (log/debug "mapped existing-child" existing-child)
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
        (let [_ (log/debug "rendering element")
              ;; Render one level
              rendered-element (render existing parent element)]
          (log/debug "rendered element" (str (get rendered-element :type)))
          (if false #_existing
            (zc/assoc-children rendered-element (zc/map-children (fn [child existing-child]
                                                                   (log/debug "mapped child" (str child))
                                                                   (log/debug "mapped existing-child" (str existing-child))
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
    (string? element)
      [element]
    (keyword? (get element :type))
      (mapcat identity (zc/map-children extract-native-elements element))
    :default
      (map extract-native-elements (zc/element-children element))))

;; Renders component into container. Does not call refresh! on container
(defn render-into-container
  [target render-state component]
  (let [root-element (render-component render-state component)
        group-info (zt/groups target)
        layer-info (layer-info group-info)
        [type {:keys [zaffre/children]} :as terminal-element]
          (render
            {:style default-style}
            render-state
            component)
        groups children]
    (log/trace "render-into-container" terminal-element)
    (assert (= type :terminal)
            (format "Root component not :terminal found %s instead" type))
    ;; for each group in terminal
    (doseq [[type {:keys [group-id pos zaffre/children]}] groups
            :let [layers children
                  {:keys [columns rows]} (get group-info group-id)]]
      (assert (= type :group)
              (format "Expected :group found %s instead" type))
      (log/info "rendering group" group-id)
      ;; update group pos
      (when pos
        (zt/alter-group-pos! target group-id pos))
      ;; for each layer in group
      (doseq [[type {:keys [layer-id zaffre/style]} :as layer] layers]
        (assert (= type :layer)
                (format "Expected :layer found %s instead" type))
        ;; create a container to hold cells
        (let [layer-container (object-array rows)
              ;; merge layer width height into layer's style
              {:keys [columns rows]} (get layer-info layer-id)
              style (merge style {:width columns :height rows})]
          (doseq [i (range rows)]
            (aset layer-container i (object-array columns)))
          (log/info "render-into-container - layer" layer-id)
          ;; render layer into layer-container
          (render-layer-into-container layer-container  (assoc-in layer [1 :zaffre/style] style))
          ;; replace chars in layer
          (zt/replace-chars!  layer-id layer-container))))))

