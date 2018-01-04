(ns zaffre.components.render-test
  (:require
    [zaffre.components.render :as zcr]
    [clojure.test :refer :all]))

(deftest flatten-test
  (are [in out] (= out (zcr/flatten-text {} in))
    ;; empty text
    [:text {:children []}] 
    [] 

    ;; split strings into words
    [:text {:children ["hello world"]}]
    [
      [:text {:children ["hello"]}]
      [:text {:children [" "]}]
      [:text {:children ["world"]}]]))

(deftest wrap-lines-test
  (are [in out] (= out in)
    ;; base cases
    (zcr/wrap-lines 20 {} [:text {:children []}])
    []
    ;; single word
    (zcr/wrap-lines 20 {} [:text {:children ["hello"]}])
    [[[:text {:children ["hello"]}]]]

    ;; split words
    (zcr/wrap-lines 20 {} [:text {:children ["hello world"]}])
    [[[:text {:children ["hello"]}]
      [:text {:children [" "]}]
      [:text {:children ["world"]}]]]

    ;; wrap line
    (zcr/wrap-lines 11 {} [:text {:children ["hello world foo bar baz qux"]}])
    [[[:text {:children ["hello"]}]
      [:text {:children [" "]}]]
     [[:text {:children ["world"]}]
      [:text {:children [" "]}]
      [:text {:children ["foo"]}]]
     [[:text {:children ["bar"]}]
      [:text {:children [" "]}]
      [:text {:children ["baz"]}]
      [:text {:children [" "]}]]
     [[:text {:children ["qux"]}]]]
))
