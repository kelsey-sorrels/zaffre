(ns zaffre.components.lifecycles-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.render :as zcr]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

;; Based on react/packages/react-dom/src/__tests__/ReactDOMServerLifecycles-test.js

(deftest should-throw-when-render-is-not-specified
  (is (thrown? AssertionError (zc/create-react-class {}))))

(deftest should-work-with-object-get-initial-state-return-values
  (binding [zc/*updater* (zc/empty-state)]
    (let [record (atom [])
          Inner (zc/create-react-class {
            :display-name "Inner"
            :component-will-mount (fn [this] (swap! record #(conj % "inner: component-will-mount")))
            :render (fn [this]
                        (swap! record #(conj % "inner: render"))
                        (zc/csx [:div]))})
          Outer (zc/create-react-class {
            :display-name "Inner"
            :component-will-mount (fn [this] (swap! record #(conj % "outer: component-will-mount")))
            :render (fn [this]
                        (swap! record #(conj % "outer: render"))
                        (zc/csx [Inner]))})
          elem (zc/csx [Outer])]
      (zc/update-state! zc/*updater*)
      (zcr/render-recursively nil nil elem)
      (is (= @record
             ["outer: component-will-mount"
              "outer: render"
              "inner: component-will-mount"
              "inner: render"])))))
