(ns zaffre.components.layout-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.layout :as zcl]
    [clojure.test :refer :all]))

(deftest layout-test
  (are [in out] (= out (zcl/layout-element in))
    ;; empty text
    [:text {:zaffre/children []}] 
    [:text {:zaffre/children []
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 0.0
              :height 1.0}}]

    ;; nested test
    [:layer {:layer-id :main
             :zaffre/style {:width 10}
             :zaffre/children [
               [:text {:zaffre/children ["16"]}]
               [:view {:zaffre/style {
                         :left 1
                         :top 1
                         :width 16}
                       :zaffre/children [
                         [:text {:zaffre/children [
                           [:text {:zaffre/children ["Lorem ipsum dolor sit amet"]}]]}]]}]]}]

    [:layer {:layer-id :main
             :zaffre/style {:width 10}
             :zaffre/layout {
               :x 0.0
               :y 0.0
               :width 10.0
               :height 0.0}
             :zaffre/children [
               [:text {:zaffre/layout {
                         :x 0.0
                         :y 0.0
                         :width 10.0
                         :height 0.0}
                       :zaffre/children ["16"]}]
               [:view {:zaffre/style {
                         :left 1
                         :top 1
                         :width 16}
                       :zaffre/layout {
                         :x 0.0
                         :y 0.0
                         :width 16.0
                         :height 0.0}
                       :zaffre/children [
                         [:text {:zaffre/layout {
                                   :x 0.0
                                   :y 0.0
                                   :width 16.0
                                   :height 0.0}
                                 :zaffre/children [
                           [:text {:zaffre/layout {
                                   :x 0.0
                                   :y 0.0
                                   :width 16.0
                                   :height 0.0}
                                 :zaffre/children ["Lorem ipsum dolor sit amet"]}]]}]]}]]}]))

(deftest absolute-position-test
  (are [in out] (= out (zcl/layout-element in))
    [:root {:zaffre/style {
              :width 100
              :height 100
              :direction :ltr}
            :zaffre/children [
              [:child {:zaffre/style {
                        :position-type :absolute
                        :position-start 10
                        :position-top 10
                        :width 10
                        :height 10}}]]}]
    
    [:root {:zaffre/style {
              :width 100
              :height 100
              :direction :ltr}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}
            :zaffre/children [
              [:child {:zaffre/style {
                        :position-type :absolute
                        :position-start 10
                        :position-top 10
                        :width 10
                        :height 10}
                       :zaffre/layout {
                         :x 10.0
                         :y 10.0
                         :width 10.0
                         :height 10.0}}]]}]))

(deftest align-items-test
  (are [in out] (= out (zcl/layout-element in))
    ;; test_align_baseline_double_nested_child
    [:root {:zaffre/style {
              :width 100
              :height 100
              :flex-direction :row
              :align-items :baseline
              :direction :ltr}
            :zaffre/children [
              [:child0 {:zaffre/style {
                          :width 50
                          :height 50}
                        :zaffre/children [
                          [:child00 {:zaffre/style {
                                      :width 50
                                      :height 20}}]]}]
              [:child1 {:zaffre/style {
                          :width 50
                          :height 20}
                        :zaffre/children [
                          [:child10 {:zaffre/style {
                                      :width 50
                                      :height 15}}]]}]]}]
                        
    [:root {:zaffre/style {
              :width 100
              :height 100
              :flex-direction :row
              :align-items :baseline
              :direction :ltr}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}
            :zaffre/children [
              [:child0 {:zaffre/style {
                          :width 50
                          :height 50}
                        :zaffre/layout {
                          :x 0.0
                          :y 0.0
                          :width 50.0
                          :height 50.0}
                        :zaffre/children [
                          [:child00 {:zaffre/style {
                                      :width 50
                                      :height 20}
                                     :zaffre/layout {
                                       :x 0.0
                                       :y 0.0
                                       :width 50.0
                                       :height 20.0}}]]}]
              [:child1 {:zaffre/style {
                          :width 50
                          :height 20}
                        :zaffre/layout {
                          :x 50.0
                          :y 5.0
                          :width 50.0
                          :height 20.0}
                        :zaffre/children [
                          [:child10 {:zaffre/style {
                                      :width 50
                                      :height 15}
                                     :zaffre/layout {
                                       :x 50.0
                                       :y 5.0
                                       :width 50.0
                                       :height 15.0}}]]}]]}]))

(deftest border-test
  (are [in out] (= out (zcl/layout-element in))
    ;; test_border_center_child 
    [:root {:zaffre/style { :justify :center
              :align-items :center
              :border-start 10
              :border-end 20
              :border-bottom 20
              :width 100
              :height 100}
            :zaffre/children [
              [:child {:zaffre/style {
                        :width 10
                        :height 10}}]]}]

    [:root {:zaffre/style {
              :justify :center
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
              :height 100.0}
            :zaffre/children [
              [:child {:zaffre/style {
                        :width 10
                        :height 10}
                       :zaffre/layout {
                        :x 40.0
                        :y 35.0
                        :width 10.0
                        :height 10.0}}]]}]))

(deftest border-test
  (are [in out] (= out (zcl/layout-element in))
    ;; test_flex_direction_row_reverse
    [:root {:zaffre/style {
              :direction :ltr
              :flex-direction :row-reverse
              :width 100
              :height 100}
            :zaffre/children [
              [:child0 {:zaffre/style {
                        :width 10}}]
              [:child1 {:zaffre/style {
                        :width 10}}]
              [:child2 {:zaffre/style {
                        :width 10}}]]}]

    [:root {:zaffre/style {
              :direction :ltr
              :flex-direction :row-reverse
              :width 100
              :height 100}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 100.0
              :height 100.0}
            :zaffre/children [
              [:child0 {:zaffre/style {
                          :width 10}
                        :zaffre/layout {
                          :x 90.0
                          :y 0.0
                          :width 10.0
                          :height 100.0}}]
              [:child1 {:zaffre/style {
                         :width 10}
                        :zaffre/layout {
                          :x 80.0
                          :y 0.0
                          :width 10.0
                          :height 100.0}}]
              [:child2 {:zaffre/style {
                         :width 10}
                        :zaffre/layout {
                          :x 70.0
                          :y 0.0
                          :width 10.0
                          :height 100.0}}]]}]))

(deftest margin-test
  (are [in out] (= out (zcl/layout-element in))
    ;; test_margin_auto_left_stretching_child
    [:root {:zaffre/style {
              :align-items :center
              :direction :ltr
              :width 200
              :height 200}
            :zaffre/children [
              [:child0 {:zaffre/style {
                        :flex-grow 1
                        :flex-shrink 1
                        :flex-basis "0%"
                        :margin-left :auto}}]
              [:child1 {:zaffre/style {
                        :width 50
                        :height 50}}]]}]

    [:root {:zaffre/style {
              :align-items :center
              :direction :ltr
              :width 200
              :height 200}
            :zaffre/layout {
              :x 0.0
              :y 0.0
              :width 200.0
              :height 200.0}
            :zaffre/children [
              [:child0 {:zaffre/style {
                          :flex-grow 1
                          :flex-shrink 1
                          :flex-basis "0%"
                          :margin-left :auto}
                        :zaffre/layout {
                          :x 200.0
                          :y 0.0
                          :width 0.0
                          :height 150.0}}]
              [:child1 {:zaffre/style {
                          :width 50
                          :height 50}
                        :zaffre/layout {
                          :x 75.0
                          :y 150.0
                          :width 50.0
                          :height 50.0}}]]}]))

