(ns zaffre.components.children-test
  (:require
    [zaffre.components :as zc]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

;; Based on https://github.com/facebook/react/blob/master/packages/react/src/__tests__/ReactChildren-test.js

(deftest should-be-called-for-each-child
  (let [zero (zc/csx [:div {:key "keyZero"}])
        one nil
        two (zc/csx [:div {:key "keyTwo"}])
        three nil
        four (zc/csx [:div {:key "keyFour"}])
        instance (zc/csx [:div {} [zero one two three four]])]
    (is (= [zero one two three four] (map identity (zc/element-children instance))))))


