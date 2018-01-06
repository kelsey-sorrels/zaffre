(ns zaffre.components.render
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.terminal :as zt]))


;; Env functions

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
                :zaffre/children
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
            element  (zc/render-comp type (assoc props :zaffre/children children))]
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
  (log/info "render-text-into-container" text)
  (let [[type {:keys [zaffre/style zaffre/layout zaffre/children] :as props}] text
        {:keys [x y width height]} layout
        style (merge default-style style)
        lines (wrap-lines width
                          style
                          text)]
    (log/info "render-text-into-container lines" lines)
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
  (log/info "render-layer-into-container" layer)
  (let [descendants (vec (component-seq layer))]
    (log/info "render-layer-into-container descendants" descendants))
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

;; Renders component into container. Does not call refresh! on container
(defn render-into-container
  [target render-state component]
  (let [group-info (zt/groups target)
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
          ;; render layer into layer-container
          (render-layer-into-container layer-container  (assoc-in layer [1 :zaffre/style] style))
          ;; replace chars in layer
          (zt/replace-chars! target layer-id layer-container))))))

