(ns zaffre.components.layout-test
  (:require
    [zaffre.components.layout :as zcl]
    [clojure.test :refer :all]))

(deftest layout-test
  (are [in out] (= out (zcl/layout-element in))
    ;; empty text
    [:text {:children []}] 
    [:text {:children []
            :layout-x 0.0
            :layout-y 0.0
            :layout-width 0.0
            :layout-height 0.0}]))
