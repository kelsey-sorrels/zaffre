(ns zaffre.components.render
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.layout :as zcl]
            [zaffre.terminal :as zt]))


;; Components -> Elements -> Rendered into terminal
;; Env functions
(defn get-props-in-env [env k]
  (if (nil? env)
    nil
    (get-in env [:props k] (get-props-in-env (get env :parent) k))))

(defn wrap-env [parent child key]
  {
    :key key
    :style (get (second child) :style {})
    ;:env/props (assoc (second child)
    ;                  :children (drop 2 child))
    :parent parent
  })

(defn env-path [env]
  (if (nil? env)
    []
    (cons (get env :key) (env-path (get env :parent)))))

(defn env-path->str [env]
  (apply str (interpose " > " (reverse (env-path env)))))

(defn compute-style [env]
  (if (nil? env)
    {}
    (merge (get env :style) (compute-style (get env :parent)))))

;; Primitive elements
(def primitive-elements #{:terminal :group :layer :view :text})
(defn primitive? [component]
  (or (string? component)
      (contains? primitive-elements (first component))))

;; Recursively calculate dimensions based on size of child components
;; Finds env in render-state and returns it. If not found,
;; evaluates body and caches result in render-state
(defmacro caching-render [component render-state & body]
  `(let [component# ~component
         render-state# ~render-state
         k# component#]
     (log/trace "render-state val" render-state# (contains? (deref render-state#) k#))
     ;; either get the cached value, or generate, cache and return it
     ;; use or instead of default value for get because get's default value
     ;; is eagerly evaluaged and will swap! every time
     (or
       (get (deref render-state#) k#)
       (let [result# (do ~@body)]
         (when (> (count (deref render-state#)) 10000)
           (reset! render-state# {}))
         (log/trace "swap!-ing render-state" render-state# k#)
         (swap! render-state# #(assoc % k# result#))
         (log/trace "new render-state" (deref render-state#))
         result#))))

(declare render)
(defn render-primitive [env render-state component]
  (log/trace "render-primitive" (env-path->str env) component)
  (caching-render component render-state
    (if (string? component)
      component
      #_[:text {:children [component]}]
      (let [[type props & children] component
            wrapped-env (wrap-env env component type)]
        [type
         (assoc props
                :children
                (vec (map-indexed
                       (fn [index child]
                         (render
                           (wrap-env
                             env
                             child
                             (get props :key index))
                           render-state
                           child))
                       children)))]))))
    
(defn render-composite [env render-state component]
  (log/trace "render-composite" (env-path->str env) (vec component))
  (caching-render component render-state
    (if (string? component)
      (zc/render-comp component {})
      (let [type     (first component)
            props    (or (second component) {})
            children (drop 2 component)
            element  (zc/render-comp type (assoc props :children children))]
        (render env render-state element)))))

(defn render
  [env render-state component]
  (log/trace "render" (env-path->str env) component)
  (let [env (wrap-env env component (first component))]
    (caching-render component render-state
      ;; render the component
      (if (primitive? component)
        (render-primitive env render-state component)
        (render-composite env render-state component)))))

(def cascaded-styles
  [:fg
   :bg])

(defn cascade-style [parent child]
  (merge (juxt cascaded-styles parent) child))

(defn flatten-text [parent-style element]
  "Splits text into words and flattens all child text element"
  (lazy-seq
    (if (string? element)
      (let [words (clojure.string/split (clojure.string/trim element) #"\s+")]
        (interpose 
          [:text (assoc parent-style :children [" "])]
          (map (fn [word] [:text (assoc parent-style :children [word])])
            words)))
      (let [[type {:keys [style children]}] element]
        (if (= type :text)
          (mapcat (partial flatten-text (merge parent-style style)) children)
          (assert false (format "found non-text element %s" type)))))))

(defn text-length [text-element]
  (-> text-element second :children first count))

(defn space? [text-element]
  (= " " (-> text-element second :children first)))

;; Adapted from https://www.rosettacode.org/wiki/Word_wrap#Clojure
(defn wrap-lines [size style text]
  "Text elements may only contain child text elements"
  (loop [left size line [] lines []
         words (or (flatten-text style text)
                   [[:text {:children [""]}]])]
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
    (log/info "render-string-into-container" x y fg bg s)
    (loop [index 0 s s]
      (let [target-index (+ x index)]
        (when (and (seq s) (< target-index max-x))
          (aset target-line target-index {:c (first s) :fg fg :bg bg})
          (recur (inc index) (rest s))))))))

(defn render-text-into-container [target props text]
  (log/info "render-text-into-container" props text)
  (let [{:keys [top left width height fg bg]} props
        [_ {:keys [style children] :as props}] text
        lines (wrap-lines width
                          props
                          text)]
    (log/info "render-text-into-container lines" lines)
    (doseq [[index line] (map-indexed vector lines)]
      (let [offsets (cons 0 (reductions + (map (fn [word] (text-length word)) line)))]
        (doseq [[offset [_ {:keys [style children]}]] (map vector offsets line)]
          (render-string-into-container target (+ left offset) (+ top index) fg bg (first children)))))))

(defn render-view-into-container [target props]
  (log/trace "render-view-into-container" props)
  (let [{:keys [top left width height fg bg]} props]
    ;; TODO draw non-bordered-view
    ;; TODO draw bordered-view))
    nil))

;; Mirrors https://github.com/facebook/yoga/blob/master/yoga/Yoga-internal.h#L195
(def default-style
  {:left 0
   :top 0
   :fg [255 255 255 255]
   :bg [0 0 0 255]

   :width 0
   :height 0

   :flex 0

   ;:margin-left 0
   ;:margin-top 0
   ;:margin-Right 0
   ;:margin-bottom 0
   ;:padding-left 0
   ;:padding-top 0
   ;:padding-right 0
   ;:padding-bottom 0
   ;:border-left 0
   ;:border-top 0
   ;:border-right 0
   ;:border-bottom 0
   :flex-direction :column
})

(defn style-element-tree [parent-env element]
  (zc/walk-elements
    identity
    (fn [[_ parent-props] [child-type child-props]]
      [child-type
       (merge child-props (cascade-style parent-props child-props))])
    element))

(defn element-seq [element]
  (log/info "element-seq" element)
  (tree-seq (fn [[_ {:keys [children]}]] (not (empty? children)))
            (fn [[_ {:keys [children]}]] children)
            element))

(defn render-layer-into-container
  [target layer-info layer]
  (log/trace "render-layer-into-container" layer-info layer)
  (let [{:keys [columns rows]} layer-info
        env {:width columns
             :height rows}
        styled-tree (style-element-tree env layer)
        laid-out-tree (zcl/layout-element styled-tree)]
    (log/info "styled-tree" styled-tree)
    (doseq [element (element-seq laid-out-tree)]
      (cond
        ;; render text
        (= :text (first element))
          (let [[type {:keys [children] :as props}] element]
            (doseq [child children]
              (render-text-into-container target props child)))
        ;; render views
        (= :view (first element))
          (let [[type {:keys [children] :as props}] element]
            (doseq [child children]
              (render-view-into-container target props child)))
        ;; no-op :layer
        (= :layer (first element))
          nil
        ;; die on other elements
        :default
          (assert false (format "Found unknown element %s" element))))))
                    
(defn layer-info [group-info]
  "Given a terminal's group info, create a map layer-id->{:columns :rows}."
  (into {}
    (mapcat identity
      (map (fn [{:keys [layers columns rows]}]
             (map (fn [layer-id]
                    [layer-id {:columns columns :rows rows}])
                  layers))
           (vals group-info)))))

;; Renders component into container. Does not call refresh! on container
(defn render-into-container
  [container render-state component]
  (let [group-info (zt/groups container)
        layer-info (layer-info group-info)
        [type {:keys [children]} :as terminal-element] (render {:style default-style}
                                                               render-state
                                                               component)
        groups children]
    (log/trace "render-into-container" terminal-element)
    (assert (= type :terminal)
            (format "Root component not :terminal found %s instead" type))
    ;; for each group in terminal
    (doseq [[type {:keys [group-id pos children]}] groups
            :let [layers children
                  {:keys [columns rows]} (get group-info group-id)]]
      (assert (= type :group)
              (format "Expected :group found %s instead" type))
      ;; update group pos
      (when pos
        (zt/alter-group-pos! container group-id pos))
      ;; for each layer in group
      (doseq [[type {:keys [layer-id]} :as layer] layers]
        (assert (= type :layer)
                (format "Expected :layer found %s instead" type))
        ;; create a container to hold cells
        (let [layer-container (object-array rows)]
          (doseq [i (range rows)]
            (aset layer-container i (object-array columns)))
          ;; render layer into layer-container
          (render-layer-into-container layer-container (get layer-info layer-id) layer)
          ;; replace chars in layer
          (zt/replace-chars! container layer-id layer-container))))))

