(ns zaffre.components.render-test
  (:require
    [zaffre.components.render :as zcr]
    [clojure.test :refer :all]))

(deftest flatten-test
  (are [in out] (= out (zcr/flatten-text {} in))
    ;; empty text
    [:text {:zaffre/children []}] 
    [] 

    ;; split strings into words
    [:text {:zaffre/children ["hello world"]}]
    [
      [:text {:zaffre/children ["hello"]}]
      [:text {:zaffre/children [" "]}]
      [:text {:zaffre/children ["world"]}]]))

(deftest wrap-lines-test
  (are [in out] (= out in)
    ;; base cases
    (zcr/wrap-lines 20 {} [:text {:zaffre/children []}])
    []
    ;; single word
    (zcr/wrap-lines 20 {} [:text {:zaffre/children ["hello"]}])
    [[[:text {:zaffre/children ["hello"]}]]]

    ;; split words
    (zcr/wrap-lines 20 {} [:text {:zaffre/children ["hello world"]}])
    [[[:text {:zaffre/children ["hello"]}]
      [:text {:zaffre/children [" "]}]
      [:text {:zaffre/children ["world"]}]]]

    ;; wrap line
    (zcr/wrap-lines 11 {} [:text {:zaffre/children ["hello world foo bar baz qux"]}])
    [[[:text {:zaffre/children ["hello"]}]
      [:text {:zaffre/children [" "]}]]
     [[:text {:zaffre/children ["world"]}]
      [:text {:zaffre/children [" "]}]
      [:text {:zaffre/children ["foo"]}]]
     [[:text {:zaffre/children ["bar"]}]
      [:text {:zaffre/children [" "]}]
      [:text {:zaffre/children ["baz"]}]
      [:text {:zaffre/children [" "]}]]
     [[:text {:zaffre/children ["qux"]}]]]
))

(deftest component-seq-test
  (are [in out] (= out (zcr/component-seq in))
    [:layer {:layer-id :main, :children [
      [:text {:children ["16"]}]
      [:view {:zaffre/style {:left 1, :top 1, :width 16},
              :children [
        [:text {:children [
          [:text {:children ["Lorem ipsum dolor sit amet"]}]]}]]}]],
             :zaffre/style {:width 30, :height 40}}]

    [[:layer {:layer-id :main, :children [
      [:text {:children ["16"]}]
      [:view {:zaffre/style {:left 1, :top 1, :width 16},
              :children [
        [:text {:children [
          [:text {:children ["Lorem ipsum dolor sit amet"]}]]}]]}]],
             :zaffre/style {:width 30, :height 40}}]]))

