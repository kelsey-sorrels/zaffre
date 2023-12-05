(ns zaffre.components.layout
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc])
  (:import (org.lwjgl.util.yoga Yoga YGNode)))


(defn build-yoga-tree [parent index element]
  (let [[_ {:keys [children]}] element
        node (Yoga/YGNodeNew)]
    (Yoga/YGNodeStyleSetFlexDirection node Yoga/YGFlexDirectionColumn)

    (Yoga/YGNodeStyleSetHeight node 100.0)
    (Yoga/YGNodeStyleSetWidth node 100.0)
    (Yoga/YGNodeStyleSetFlex node 1.0)

    (when parent
      (Yoga/YGNodeInsertChild parent node index))
    (doseq [[index child] (map-indexed vector children)]
      (build-yoga-tree node index child))
    node))

(defn free-yoga-tree [yoga-tree]
  nil)

(defn transfer-layout [yoga-tree element]
  "Traverses the yoga tree and element simultaneously updating the element with
   layout information"
  element)

(defn layout-element [element]
  (let [yoga-tree (build-yoga-tree nil nil element)]
    (try
      (Yoga/YGNodeCalculateLayout yoga-tree Yoga/YGUndefined Yoga/YGUndefined Yoga/YGDirectionLTR)
      (transfer-layout yoga-tree element)
      (finally
        (free-yoga-tree yoga-tree)))))

