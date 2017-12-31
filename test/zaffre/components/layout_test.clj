(ns zaffre.components.layout-test
  (:require
    [zaffre.components.layout :as zcl]
    [clojure.test :refer :all]))

(deftest layout-test
  (are [in out] (= out (zcl/layout-element in))
    ;; empty text
    [:text {:children []}] 
    [:text {:children []}]))
