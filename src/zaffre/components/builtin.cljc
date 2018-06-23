(ns zaffre.components.builtin)

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
  (let [{:keys [zaffre/children]} props]
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
  (let [{:keys [width height style zaffre/children]} props
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
  (let [{:keys [width height zaffre/children]} props]
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
