(ns zaffre.components.layout
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc])
  (:import (org.lwjgl.system MemoryStack)
           (org.lwjgl.util.yoga
             YGMeasureFunc
             YGMeasureFuncI
             YGSize
             Yoga)))

(defmulti measure-text (fn [width width-mode height height-mode text]
  (cond
    (and (= width-mode Yoga/YGMeasureModeUndefined)
         (= height-mode Yoga/YGMeasureModeUndefined))
      :unrestricted
    (= width-mode Yoga/YGMeasureModeUndefined)
      :width-restricted
    (= height-mode Yoga/YGMeasureModeUndefined)
      :height-restricted
    :default
      :bi-restricted)))

(defmethod measure-text :unrestricted
  [width width-mode height height-mode text]
    [(count text) 1])

(defmethod measure-text :width-restricted
  [width width-mode height height-mode text]
    [(count text) 1])

(defmethod measure-text :height-restricted
  [width width-mode height height-mode text]
    [(count text) 1])

(defmethod measure-text :bi-restricted
  [width width-mode height height-mode text]
    [(count text) 1])

(defn text-measure-func [text]
  (YGMeasureFunc/create (reify YGMeasureFuncI
    (invoke [_ node width width-mode height height-mode]
      (let [[result-width result-height] (measure-text width width-mode height height-mode text)]
        (with-open[stack (MemoryStack/stackPush)]
            (-> (YGSize/mallocStack stack)
                (.width result-width)
                (.height result-height)
                YGMeasureFunc/toLong)))))))

(defn build-yoga-tree [parent index element]
  "Takes [type props] elements and annotates them with yoga nodes like
   [type props node]. Does this recursively to the element's children"
  (log/info "build-yoga-tree" index element)
  (let [[type {:keys [children] :as props}] element
        node (Yoga/YGNodeNew)]
    (Yoga/YGNodeStyleSetFlexDirection node Yoga/YGFlexDirectionColumn)

    #_(Yoga/YGNodeStyleSetHeight node 100.0)
    #_(Yoga/YGNodeStyleSetWidth node 100.0)
    #_(Yoga/YGNodeStyleSetFlex node 1.0)

    (when parent
      (Yoga/YGNodeInsertChild parent node index))
 
    (when (and (= type :text) (every? string? children))
      (Yoga/YGNodeSetMeasureFunc node (text-measure-func (first children))))

    (log/info "node" node)
    [type
     (assoc props :children
       ;; force eager evaluation so that entire tree is created
       (vec (map-indexed (fn [index child]
         (if (sequential? child)
           (build-yoga-tree node index child)
           child))
         children)))
     node]))

(defn transfer-layout [yoga-tree]
  "Traverses the yoga tree updating the element portion with
   layout information and frees the yoga node"
  (log/info "transfer-layout" yoga-tree)
  (if (and (sequential? yoga-tree) (= 3 (count yoga-tree)))
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
         :children (map transfer-layout children))])
    yoga-tree))

(defn layout-element [element]
  (let [yoga-tree (build-yoga-tree nil nil element)
        root-node (last yoga-tree)]
    (log/info "yoga-tree" yoga-tree)
    (Yoga/YGNodeCalculateLayout root-node Yoga/YGUndefined Yoga/YGUndefined Yoga/YGDirectionLTR)
    (transfer-layout yoga-tree)))

