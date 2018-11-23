(ns zaffre.components.class-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.render :as zcr]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

;; Based on react/packages/react/src/__tests__/ReactES6Class-test.js

(deftest should-throw-when-render-is-not-specified
  (is (thrown? AssertionError (zc/create-react-class {}))))

(deftest should-work-with-object-get-initial-state-return-values
  (binding [zc/*updater* (zc/empty-state)]
    (let [Component (zc/create-react-class {
                  :display-name "Component"
                  :get-initial-state (fn [] {:occupation "clown"})
                  :render (fn [this]
                              (zc/csx [:div]))})
          elem (zc/csx [Component])]
      (zc/update-state! zc/*updater*)
      (zcr/render-recursively nil nil elem)
      (is (= (get (zc/get-state zc/*updater* elem) :occupation) "clown")))))
  
(deftest should-work-with-get-derived-state-from-props-return-values
  (binding [zc/*updater* (zc/empty-state)]
    (let [Component (zc/create-react-class {
                  :display-name "Component"
                  :get-initial-state (fn [] {})
                  :get-derived-state-from-props (fn [this next-props prev-state] {:occupation "clown"})
                  :render (fn [this]
                            (zc/csx [:div]))})
          elem (zc/csx [Component])
          rendered-elem (zcr/render-recursively nil nil elem)]
      (zc/update-state! zc/*updater*)
      (is (= "clown" (get(zc/get-state zc/*updater* rendered-elem) :occupation))))))

(deftest should-get-derived-state-from-props-updates-state-when-props-change
  (binding [zc/*updater* (zc/empty-state)]
    (let [Component (zc/create-react-class {
                  :display-name "Component"
                  :get-initial-state (fn [] {:count 1})
                  :get-derived-state-from-props
                    (fn [this next-props prev-state]
                      {:count (+ (get prev-state :count) (get next-props :increment-by))})
                  :render (fn [this]
                            (let [{:keys [count]} (zc/state this)
                                  value (str "count:" count)]
                            (zc/csx [:text {} [value]])))})
          elem (zc/csx [Component {:increment-by 1}])
          rendered-elem (zcr/render-recursively nil nil elem)]
      (is (= "count:1" (first (get-in rendered-elem [:props :children 0 :props :children ])))))))

  
