(ns zaffre.components.layout
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc])
  (:import (org.lwjgl.util.yoga Yoga YGNode)))


(defn build-yoga-tree [parent index element]
  "Takes [type props] elements and annotates them with yoga nodes like
   [type props node]. Does this recursively to the element's children"
  (let [[type {:keys [children] :as props}] element
        node (Yoga/YGNodeNew)]
    (Yoga/YGNodeStyleSetFlexDirection node Yoga/YGFlexDirectionColumn)

    #_#_#_(Yoga/YGNodeStyleSetHeight node 100.0)
    (Yoga/YGNodeStyleSetWidth node 100.0)
    (Yoga/YGNodeStyleSetFlex node 1.0)

    (when parent
      (Yoga/YGNodeInsertChild parent node index))
    [type
     (assoc props :children
       (map-indexed (fn [index child]
         (build-yoga-tree node index child))
         children))
     node]))

(defn free-yoga-tree [yoga-tree]
  (let [[_ {:keys [children]} node] yoga-tree]
    (doseq [child children]
      (free-yoga-tree child))
    (Yoga/YGNodeFree yoga-tree)))

(defn transfer-layout [yoga-tree]
  "Traverses the yoga tree updating the element portion with
   layout information and frees the yoga node"
  (let [[type {:keys [children] :as props} node] yoga-tree
        layout-x (Yoga/YGNodeLayoutGetLeft node)
        layout-y (Yoga/YGNodeLayoutGetTop node)
        layout-width (Yoga/YGNodeLayoutGetWidth node)
        layout-height (Yoga/YGNodeLayoutGetHeight node)]
    (Yoga/YGNodeFree node)
    [type
     (assoc props
       :layout-x layout-x
       :layout-y layout-y
       :layout-width layout-width
       :layout-height layout-height
       :children (map transfer-layout children))]))

(defn layout-element [element]
  (let [yoga-tree (build-yoga-tree nil nil element)
        root-node (last yoga-tree)]
    (Yoga/YGNodeCalculateLayout root-node Yoga/YGUndefined Yoga/YGUndefined Yoga/YGDirectionLTR)
    (transfer-layout yoga-tree)))

