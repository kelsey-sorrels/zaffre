(ns zaffre.components.render
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.terminal :as zt]))


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

(defn merge-envs [env other]
  {:fg (or (get other :fg) (get env :fg))
   :bg (or (get other :bg) (get env :bg))
   :left  (+ (get env :left 0) (get other :left 0))
   :top  (+ (get env :top 0) (get other :top 0))})

(defn flatten-text [parent-style element]
  "Splits text into words and flattens all child text element"
  (println "flatten-text" element)
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
  (println "text-length" text-element)
  (-> text-element second :children first count))

(defn space? [text-element]
  (= " " (-> text-element second :children first)))

;; Adapted frmo https://www.rosettacode.org/wiki/Word_wrap#Clojure
(defn wrap-lines [size style text]
  "Text elements may only contain child text elements"
  (loop [left size line [] lines []
         words (or (flatten-text style text)
                   [[:text {:children [""]}]])]
    (log/info "wrap-lines words" left style (vec words))
    (if-let [word (first words)]
      (if (and (= left size) (space? word))
        (recur size [] lines (next words))
        (let [wlen (text-length word)
              spacing (if (== left size) "" " ")
              alen (+ (count spacing) wlen)]
          (log/info "wlen" wlen "spacing" spacing "alen" alen)
          (if (<= alen left)
            (recur (- left alen) (conj line word) lines (next words))
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

(defn render-text-into-container [target parent-layout env text]
    (log/info "render-text-into-container" parent-layout env text)
  (let [{:keys [columns rows]} parent-layout
        {:keys [top left fg bg]} env
        [type {:keys [style children] :as props}] text
        style (compute-style env)
        lines (wrap-lines columns
                          style
                          text)]
    (log/info "render-text-into-container lines" lines)
    (doseq [[index line] (map-indexed vector lines)]
      (let [offsets (cons 0 (reductions + (map (fn [word] (text-length word)) line)))]
        (doseq [[offset [_ {:keys [style children]}]] (map vector offsets line)]
          (render-string-into-container target (+ left offset) (+ top index) fg bg (first children)))))))

(def default-style
  {:left 0
   :top 0
   :fg [255 255 255 255]
   :bg [0 0 0 255]

   ;:width nil
   ;:height nil
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
   ;:flex-direction :column
})


(defmulti merge-linear-dimension (fn [parent child]
  (cond
    (number? child) :absolute
    (nil? child) :default
    (contains? child :%) :percent)))
(defmethod merge-linear-dimension :absolute [parent child] (min parent child))
(defmethod merge-linear-dimension :default [parent _] parent)
(defmethod merge-linear-dimension :percent [parent child] (int (* parent (/ (get child :%) 100.0))))

(defn- merge-layouts [parent-layout layout]
  {:columns (let [parent-columns (get parent-layout :columns)
                  child-columns (get layout :columns parent-columns)]
              (merge-linear-dimension parent-columns child-columns))
   :rows (let [parent-rows (get parent-layout :rows)
                  child-rows (get layout :rows parent-rows)]
              (merge-linear-dimension parent-rows child-rows))})

(defn render-layer-into-container
  ([target layer-info layer]
    (let [[_ {:keys [children]}] layer]
      (doseq [child children]
        (render-layer-into-container target layer-info default-style child))))
  ([target parent-layout env component]
    (log/info "render-layer-into-container" parent-layout env component)
    (cond
      ;; render strings
      (= :text (first component))
        (let [[type {:keys [children]} :as props] component]
          (doseq [child children]
            (render-text-into-container target parent-layout env child)))
      ;; render views
      (= :view (first component))
        (let [[type {:keys [children] :as props}] component
              merged-layout (merge-layouts parent-layout props)
              merged-env (merge-envs env props)]
          (doseq [child children]
            (render-layer-into-container target merged-layout merged-env child)))
      ;; die on other components
      :default
        (assert false (format "Found unknown component %s" component)))))
                    
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

