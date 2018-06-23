(ns zaffre.components.component-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.render :as zcr]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :debug)

;; Based on https://github.com/facebook/react/blob/master/packages/react-dom/src/__tests__/ReactDOMComponent-test.js

#_(deftest should-throw-for-unknown-function-event-handlers
  (is (thrown? AssertionError (zc/csx [:div {:on-unknown identity}]))))

#_(deftest should-properly-update-custom-attributes-on-custom-elements
  (let [elem1 (zc/csx [:div {:foo "bar"}])
        elem2 (zc/csx [:div {:bar "buzz"}])
        rendered (zcr/render-recursively nil nil elem1)
        re-rendered (zcr/render-recursively rendered nil elem2)]
   (log/info re-rendered)
   (is (nil? (get-in re-rendered [:props :foo])))
   (is (= "buzz" (get-in re-rendered [:props :bar])))))

;; State tests
#_(deftest should-properly-render-using-initial-state
  (let [A (zc/create-react-class {
             :display-name "A"
             :get-initial-state (fn [] {:value "hello world"})
             :render (fn [this]
                       (let [{:keys [value]} (zc/state this)]
                         (log/info "render fn. state" (zc/state this))
                         (zc/csx [:div {} [value]])))})
        elem (zc/csx [A])
        rendered-elem (zcr/render-recursively nil nil elem)]
    (println (zc/print-tree rendered-elem))
    (is (= (get-in rendered-elem [:props :children 0 :props :children] "hello world")))))

(deftest should-properly-re-render-on-state-change
  (log/set-level! :warn)
  (let [A (zc/create-react-class {
             :display-name "A"
             :get-initial-state (fn [] {:value "hello world"})
             :render (fn [this]
                       (let [{:keys [value]} (zc/state this)]
                         (log/warn "render fn. state" (zc/state this))
                         (zc/csx [:div {} [value]])))})
        elem (zc/csx [A])
        rendered-elem (zcr/render-recursively nil nil elem)]
    (println (zc/print-tree rendered-elem))
    (reset! (get elem :state) {:value "hola mundo"})
    (log/set-level! :debug)
    (let [re-rendered-elem (zcr/render-recursively rendered-elem nil elem)]
      (println (zc/print-tree re-rendered-elem)))))

#_(deftest should-properly-re-render-child-on-state-change
  (log/set-level! :warn)
  (let [Child (zc/create-react-class {
                :display-name "Child"
                :get-initial-state (fn [] {:value "hello world"})
                :render (fn [this]
                          (let [{:keys [value]} (zc/state this)]
                            (zc/csx [:div {} [value]])))})
        child-elem (zc/csx [Child])
        Parent (zc/fn->component (fn [this] child-elem) "Parent")
        parent-elem (zc/csx [Parent])
        rendered-elem (zcr/render-recursively nil nil parent-elem)]
    (println (zc/print-tree rendered-elem))
    (swap! (get-in child-elem [:type :state]) {:value "hola mundo"})
    (log/set-level! :debug)
    (let [re-rendered-elem (zcr/render-recursively rendered-elem nil parent-elem)]
      (println (zc/print-tree re-rendered-elem)))))
