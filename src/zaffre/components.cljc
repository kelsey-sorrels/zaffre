(ns zaffre.components)

;; Util
(defn with-children [type props children]
  (cons type
    (cons props
      children)))

;; Env functions
(defn get-props-in-env [env k]
  (if (nil? env)
    nil
    (get-in env [:env/props k] (get-props-in-env (get env :env/parent) k))))

(defn wrap-env [child parent key]
  {
    :env/key key
    :env/child child
    :env/props (assoc (second child)
                      :children (drop 2 child))
    :env/parent parent
  })

(defn env-path [env]
  (if (nil? env)
    []
    (cons (get env :env/path) (env-path (get env :env/parent)))))

(defn env-path->str [env]
  (str (interpose (reverse (env-path env)) " > ")))

;;; Element methods
;; Elements are [type props] where props contain :children
(defn walk-elements-indexed [parent-fn child-fn element]
  "Walks an element tree applying parent-fn to the root, and then calling (child-fn root index child) to each child
   replacing them."
  (let [[type {:keys [children] :as props} :as new-element] (parent-fn element)]
    (assoc element
      0 type
      1 (assoc props :children
          (map-indexed (fn [index child]
            (if (seq? child)
              (walk-elements-indexed parent-fn child-fn (child-fn new-element index child))
              child))
            children)))))

(defn walk-elements [parent-fn child-fn element]
  "Walks an element tree applying parent-fn to the root, and then calling (child-fn root child) to each child
   replacing them"
  (walk-elements-indexed parent-fn (fn [parent _ child] (child-fn parent child)) element))
  

;;; Component methods
;; Components are 

;; Find the dimensions of a component
(defmulti dimensions (fn [component]
  (if (string? component)
    :raw-string
    (first component))))
;; Given [type prop], render a component
(defmulti render-comp (fn [type props]
  (cond
    (string? type) :raw-string
    (keyword? type) type)))

(defmethod render-comp :default [type props]
  (assert false (format "render-comp expected string or keyword found %s" type)))

(defmulti should-component-update?
  (fn [env _]
    "[env render-state]. Dispatches based on component type"
    (first (get env :env/child))))
;; default should-component-update? updates only when a component's props
;; have changed
(defmethod should-component-update? :default [env render-state]
  true)
    ;(contains? render-state [(env-path env) (get env :env/props)]))

;; Raw-String
(defmethod dimensions :raw-string [component]
  [(count component) 1])
(defmethod render-comp :raw-string [type props]

;; Text
(defmethod dimensions :text [component]
  (let [type (first component)]
    [(count type) 1]))
  [:text {} type])
(defmethod render-comp :text [type props]
  [type props])

;; View
(defmethod dimensions :view [component]
  (let [type (first component)
        {:keys [width height]} (second component)
        children (drop 2 component)
        child-dimensions (map dimensions children)]
    [(or width
         (reduce + 0 (map first child-dimensions)))
     (or height
         (reduce max (map second child-dimensions)))]))

;; Label
(defmethod dimensions :label [component]
  (let [type (first component)
        children (drop 2 component)
        child-dimensions (map dimensions children)]
    [(reduce + 0 (map first child-dimensions))
     (reduce max (map second child-dimensions))]))
(defmethod render-comp :label [type props]
  (let [{:keys [children]} props]
    (with-children
      :view
      props
      (let [offsets (cons 0 (reductions + (map (fn [child] (first (dimensions child))) children)))]
        (map (fn child-render [x child]
               [:view {:left x} child])
             offsets
             children)))))
        
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

(defmethod dimensions :border [component]
  (let [children (drop 2 component)
        child-dimensions (map dimensions children)]
    [(reduce + 0 (map first child-dimensions))
     (reduce max (map second child-dimensions))]))
(defmethod render-comp :border [type props]
  (let [{:keys [width height style children]} props
        {:keys [horizontal
                vertical
                top-left
                top-right
                bottom-left
                bottom-right]} style]
    [:view {}
    ;; render top and bottom
      (mapcat identity
          ; tl
          [top-left]
          ; t
          (for [dx (range (- width 2))]
            horizontal)
          ; tr
          [top-right]
          ; middle
          (for [dy (range (dec height))]
            (concat
             [vertical]
             (repeat (- width 2) nil)
             [vertical]))
          ; bl
          [bottom-left]
          ; b
          (for [dx (range (- width 2))]
            horizontal)
          ; br
          [bottom-right])]))

;; List view
(defmethod dimensions :list-view [component]
  (let [children (drop 2 component)
        child-dimensions (map dimensions children)]
    [(reduce max 0 (map first child-dimensions))
     (reduce + (map second child-dimensions))]))
(defmethod render-comp :list-view [type props]
  (let [{:keys [width height children]} props]
    (cons
      :view
      (cons {:left 0 :top 0 :width width :height height}
        (map-indexed
          (fn [index child]
            [:view {:top index} child])
          children)))))

;; Input
(defmethod dimensions :input [component]
  (let [children (drop 2 component)
        child-dimensions (map dimensions children)]
    [(reduce + 0 (map first child-dimensions))
     (reduce max (map second child-dimensions))]))
(defmethod render-comp :input [type props]
  (let [{:keys [value]} props]
    [:label props value]))
