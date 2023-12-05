(ns zaffre.components.events
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.terminal :as zt]))

;; Transform :text tree into a sequence by walking the tree
(defn element-tree-seq [element]
  (let [element-seq (tree-seq zc/element?
                              zc/element-children
                              element)]
    element-seq))

;; Return only the leaf nodes from text-tree-seq
(defn filter-element-tree [p element]
  (let [elements (element-tree-seq element)]
    (filter p elements)))

(defn input-element-seq [dom]
  (filter-element-tree (fn [{:keys [type]}]
                         (= type zcui/Input))
                        dom))

(defn send-events-to-dom [events dom]
  (log/info "send-events-to-dom" events)
  (let [selected-input-index 0
        input-elements (input-element-seq dom)]
    ;(log/trace "input-elements" (type input-elements) (vec input-elements))
    (when-let [selected-input-element (nth input-elements selected-input-index)]
      (let [instance (zc/construct-instance selected-input-element)
            {:keys [on-keypress]} (zc/props instance)]
        ;; call :on-* to dom elements updating state
        (log/trace "on-keypress" on-keypress #_#_"instance" instance)
        (binding [zc/*current-owner* selected-input-element]
          (on-keypress
            (assoc instance :updater zc/*updater*)
            {:key-code (int 13)
             :key \a})))))
  ;; Take all enqueued state changes and reduce them down to new state
  (zc/update-state! zc/*updater*))

