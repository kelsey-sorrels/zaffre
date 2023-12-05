(ns zaffre.components.layout-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.layout :as zcl]
    [clojure.test :refer :all]))

(defn elements=
  [e1 e2]
  (every? (fn [[v1 v2]]
            (let [vv1 (cond-> v1 (instance? clojure.lang.IDeref v1) deref)
                  vv2 (cond-> v2 (instance? clojure.lang.IDeref v2) deref)]
              (if (and (vector? vv1)
                       (vector? vv2))
                ;; vv1 and vv2 are both children
                (every? some? (map elements= vv1 vv2))
                (= vv1 vv2))))
          (map vector e1 e2)))
                        

(deftest layout-test
  (are [in out] (elements= out (zcl/layout-element in))
    ;; empty text
    [:text {} [] nil nil] 
    [:text {:style {
              :width nil
              :height nil}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 0.0
              :height 1.0}} []
      nil nil]

    ;; nested test
    [:layer {:layer-id :main
             :style {:width 10}} [
               [:text {} ["16"] nil nil]
               [:view {:style {
                         :left 1
                         :top 1
                         :width 16}}
                       [
                         [:text {} [
                           [:text {} ["Lorem ipsum dolor sit amet"] nil nil] nil nil]]]
                 nil nil]] nil nil]

    [:layer {:layer-id :main
             :style {:width 10 :height nil}
             :zaffre/layout {
               :x 0.0
               :y 0.0
               :width 10.0
               :height 0.0}} [
               [:text {:zaffre/layout {
                         :x 0.0
                         :y 0.0
                         :width 10.0
                         :height 0.0}}
                       ["16"]]
               [:view {:style {
                         :left 1
                         :top 1
                         :width 16}
                       :zaffre/layout {
                         :x 1.0
                         :y 1.0
                         :width 16.0
                         :height 0.0}} [
                         [:text {:zaffre/layout {
                                   :x 1.0
                                   :y 1.0
                                   :width 16.0
                                   :height 0.0}} [
                           [:text {} ["Lorem ipsum dolor sit amet"]]]]]]]]))

(deftest absolute-position-test
  ; absolute_layout_width_height_start_top
  (are [in out] (elements= out (zcl/layout-element in))
    [:root {:style {
              :width 100
              :height 100}}
            [[:child {:style {
                        :position :absolute
                        :left 10
                        :top 10
                        :width 10
                        :height 10}} [] (atom nil)  nil]]
      (atom nil) nil]
    
    [:root {:style {
              :width 100
              :height 100}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}}
              [[:child {:style {
                          :position :absolute
                          :left 10
                          :top 10
                          :width 10
                          :height 10}
                         :zaffre/layout {
                           :x 10.0
                           :y 10.0
                           :width 10.0
                           :height 10.0}} [] (atom {
                           :x 10.0
                           :y 10.0
                           :width 10.0
                           :height 10.0}) nil]]
      (atom {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}) nil]))

(deftest align-items-test
  (are [in out] (elements= out (zcl/layout-element in))
    ;; test_align_baseline_double_nested_child
    [:root {:style {
              :width 100
              :height 100
              :flex-direction :row
              :align-items :baseline
              :direction :ltr}} [
              [:child0 {:style {
                          :width 50
                          :height 50}} [
                          [:child00 {:style {
                                      :width 50
                                      :height 20}}]]]
              [:child1 {:style {
                          :width 50
                          :height 20}} [
                          [:child10 {:style {
                                      :width 50
                                      :height 15}}]]]]]
                        
    [:root {:style {
              :width 100
              :height 100
              :flex-direction :row
              :align-items :baseline
              :direction :ltr}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}} [
              [:child0 {:style {
                          :width 50
                          :height 50}
                        :zaffre/layout {
                          :x 0.0
                          :y 0.0
                          :width 50.0
                          :height 50.0}} [
                          [:child00 {:style {
                                      :width 50
                                      :height 20}
                                     :zaffre/layout {
                                       :x 0.0
                                       :y 0.0
                                       :width 50.0
                                       :height 20.0}} []]]]
              [:child1 {:style {
                          :width 50
                          :height 20}
                        :zaffre/layout {
                          :x 50.0
                          :y 5.0
                          :width 50.0
                          :height 20.0}} [
                          [:child10 {:style {
                                      :width 50
                                      :height 15}
                                     :zaffre/layout {
                                       :x 50.0
                                       :y 5.0
                                       :width 50.0
                                       :height 15.0}} []]]]]]))

(deftest border-test-center-child
  (are [in out] (elements= out (zcl/layout-element in))
    ;; test_border_center_child 
    [:root {:style {
              :justify-content :center
              :align-items :center
              :border-start 10
              :border-end 20
              :border-bottom 20
              :width 100
              :height 100}} 
              [[:child {:style {
                        :width 10
                        :height 10}}]]]

    [:root {:style {
              :justify-content :center
              :align-items :center
              :border-start 10
              :border-end 20
              :border-bottom 20
              :width 100
              :height 100}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}} [
              [:child {:style {
                        :width 10
                        :height 10}
                       :zaffre/layout {
                        :x 40.0
                        :y 35.0
                        :width 10.0
                        :height 10.0}} []]]]))

(deftest border-test-row-reverse
  (are [in out] (elements= out (zcl/layout-element in))
    ;; test_flex_direction_row_reverse
    [:root {:style {
              :direction :ltr
              :flex-direction :row-reverse
              :width 100
              :height 100}} [
              [:child0 {:style {
                        :width 10}}]
              [:child1 {:style {
                        :width 10}}]
              [:child2 {:style {
                        :width 10}}]]]

    [:root {:style {
              :direction :ltr
              :flex-direction :row-reverse
              :width 100
              :height 100}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}} [
              [:child0 {:style {
                          :width 10}
                        :zaffre/layout {
                          :x 90.0
                          :y 0.0
                          :width 10.0
                          :height 100.0}} []]
              [:child1 {:style {
                         :width 10}
                        :zaffre/layout {
                          :x 80.0
                          :y 0.0
                          :width 10.0
                          :height 100.0}} []]
              [:child2 {:style {
                         :width 10}
                        :zaffre/layout {
                          :x 70.0
                          :y 0.0
                          :width 10.0
                          :height 100.0}} []]]]))

(deftest margin-test
  (are [in out] (elements= out (zcl/layout-element in))
    ;; test_margin_auto_left_stretching_child
    [:root {:style {
              :align-items :center
              :direction :ltr
              :width 200
              :height 200}} [
              [:child0 {:style {
                        :flex-grow 1
                        :flex-shrink 1
                        :flex-basis "0%"
                        :margin-left :auto}}]
              [:child1 {:style {
                        :width 50
                        :height 50}}]]]

    [:root {:style {
              :align-items :center
              :direction :ltr
              :width 200
              :height 200}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 200.0
              :height 200.0}} [
              [:child0 {:style {
                          :flex-grow 1
                          :flex-shrink 1
                          :flex-basis "0%"
                          :margin-left :auto}
                        :zaffre/layout {
                          :x 200.0
                          :y 0.0
                          :width 0.0
                          :height 150.0}} []]
              [:child1 {:style {
                          :width 50
                          :height 50}
                        :zaffre/layout {
                          :x 75.0
                          :y 150.0
                          :width 50.0
                          :height 50.0}} []]]]))


