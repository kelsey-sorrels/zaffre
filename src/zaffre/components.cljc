(ns zaffre.components
  (:require clojure.data
            clojure.string
            [cashmere.core-graal :as cm]
            [clojure.zip :as zip]
            [overtone.at-at :as atat]
            [taoensso.timbre :as log]))
 
(def ^:dynamic *pool* (atat/mk-pool))
(declare ^:dynamic *current-owner*)

(declare element-id-str)

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
                     (if (and (map? v1) (map? v2))
                       (merge-with deep-merge v1 v2)
                       v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

;; From https://reactjs.org/docs/events.html
(def valid-event-handler-keys #{
                                ;; keyboard events
                                :on-keydown
                                :on-keyup
                                :on-keypress
                                ;; focus events
                                :on-focus
                                :on-blur
                                ;; mouse events
                                :on-click
                                :on-double-click
                                :on-drag
                                :on-drag-end
                                :on-drag-enter
                                :on-drag-exit
                                :on-drag-leave
                                :on-drag-over
                                :on-drag-start
                                :on-drag-stop
                                :on-drop
                                :on-mouse-down
                                :on-mouse-enter
                                :on-mouse-leave
                                :on-mouse-move
                                :on-mouse-out
                                :on-mouse-over
                                :on-mouse-up
                                ;; wheel events
                                :on-wheel
                                ;; animation events
                                :on-animation-start
                                :on-animation-end
                                :on-animation-iteration
                                ;; form events
                                :on-change})

(defn is-valid-event-handler-key? [k]
  (contains? valid-event-handler-keys k))

(def default-opts {
  :component-will-mount (fn [this] nil)
  :render (fn [this] nil)
  :component-did-mount (fn [this] nil)
  :component-will-receive-props (fn [this next-props] nil)
  :should-component-update? (fn [this next-props next-state] true)
  :component-will-update (fn [this next-props next-state] nil)
  :get-snapshot-before-update (fn [this] nil)
  :component-did-update (fn [this prev-props prev-state] nil)
  :component-will-unmount (fn [this] nil)
  :component-did-catch (fn [this error info] nil)
  :get-default-props (fn [] {})
  :get-initial-state (fn [] nil)
  :get-derived-state-from-props (fn [this next-props prev-state] prev-state)})

(defn tree-edges
  ([loc]
    (tree-edges loc true))
  ([loc rightmost]
    (if loc
      (let [has-right-sibling (some? (zip/right loc))]
        (conj (tree-edges (zip/up loc) false)
              (cond
                (and rightmost has-right-sibling)
                  "\u251c" ;├
                (and rightmost (not has-right-sibling))
                  "\u2514" ; └
                (and (not rightmost) has-right-sibling)
                  "\u2502" ; │
                (and (not rightmost) (not has-right-sibling))
                  " ")))
              
      [])))

(defn zipper-elements
  [root-element]
  (zip/zipper
    ; can have children
    (fn [v] (or (cm/cashmere-instance? v) (map? v) (map-entry? v)))
    ; children
    (fn [v]
      (cond
        (cm/cashmere-instance? v)
          (vec (array-map
            :props (nth v 1)
            :host-dom @(nth v 3)
            :children (nth v 2)))
        (map? v)
          (vec v)
        (map-entry? v)
          (if (coll? (second v))
            (second v)
            [(second v)])))
    ; with children
    (fn [v children]
      (cond
        (cm/cashmere-instance? v)
          (let [[tag props _ host-dom] v]
            [tag props children host-dom])
        (map? v)
          {(ffirst v) children}
        (map-entry? v)
          (first {(first v) children})))
    root-element))


(defn zipper-descendants
  [z]
  #_(log/info "zipper-descendants" z)
  (if-not (zip/end? z)
    (lazy-seq
      (cons z (zipper-descendants (zip/next z))))
    []))

(defn tree->str [root-element]
  (clojure.string/join "\n"
    (cons "\nroot-element"
      (for [z (-> root-element zipper-elements zipper-descendants)
            :let [node (zip/node z)]]
        (str (clojure.string/join (tree-edges z))
             (cond
               (cm/cashmere-instance? node)
                 (str (first node) (nth node 3) (nth node 4))
               (map-entry? node)
                 (first node)
               (map? node)
                 {}
               (nil? node)
                 "nil"
               :else
                 node))))))
  
(defn -main [& args]
  (println 
    (clojure.string/join "\n"
      (cons "\nX"
        (for [z (-> (first {:root {:a [1 2] :b {3 "x"} :c "c"}}) zipper-elements zipper-descendants)
              :let [node (zip/node z)]]
          (str (clojure.string/join (tree-edges z))
               (cond
                 (cm/cashmere-instance? node)
                   (first node)
                 (map-entry? node)
                   (first node)
                 (map? node)
                   {}
                 :else
                   node)))))))
