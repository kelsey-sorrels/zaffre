(ns zaffre.components.component-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.render :as zcr]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

;; Based on https://github.com/facebook/react/blob/master/packages/react-dom/src/__tests__/ReactDOMComponent-test.js

(deftest should-throw-for-unknown-function-event-handlers
  (is (thrown? AssertionError (zc/csx [:div {:on-unknown identity}]))))

;; Updater tests
(deftest should-create-empty-state
  (let [s (zc/empty-state)]
    (is (-> s :a deref :s empty?))))

(deftest should-accept-state-callback
  (let [elem {:id :test}
        s  (-> (zc/empty-state)
               (zc/enqueue-set-state! elem nil #(assoc % :k :v)))]
    (is (= (-> s :a deref :q count)) 1)
    (is (= (-> s :a deref :s) {}))))

(deftest should-update-state-with-callback
  (let [elem {:id :test}
        s  (-> (zc/empty-state)
               (zc/enqueue-set-state! elem nil #(assoc % :k :v))
               zc/update-state!)]
    (is (-> s :a deref :q empty?))
    (is (= (-> s :a deref :s) {:test {:k :v}}))))

(deftest should-accept-state-partial
  (let [elem {:id :test}
        s  (-> (zc/empty-state)
               (zc/enqueue-set-state! elem {:k :v} nil))]
    (is (= (-> s :a deref :q count)) 1)
    (is (= (-> s :a deref :s) {}))))

(deftest should-update-state-with-partial
  (let [elem {:id :test}
        s  (-> (zc/empty-state)
               (zc/enqueue-set-state! elem {:k :v} nil)
               zc/update-state!)]
    (is (-> s :a deref :q empty?))
    (is (= (-> s :a deref :s) {:test {:k :v}}))))

(deftest should-properly-update-custom-attributes-on-custom-elements
  (let [elem1 (zc/csx [:div {:foo "bar"}])
        elem2 (zc/csx [:div {:bar "buzz"}])
        rendered (zcr/render-recursively nil nil elem1)
        re-rendered (zcr/render-recursively rendered nil elem2)]
   (log/info re-rendered)
   (is (nil? (get-in re-rendered [:props :foo])))
   (is (= "buzz" (get-in re-rendered [:props :bar])))))

;; State tests
(deftest should-properly-render-using-initial-state
  (binding [zc/*updater* (zc/empty-state)]
    (let [A (zc/create-react-class {
               :display-name "A"
               :get-initial-state (fn [] {:value "hello world"})
               :render (fn [this]
                         (let [{:keys [value]} (zc/state this)]
                           (log/info "render fn. state" (zc/state this))
                           (zc/csx [:div {} [value]])))})
          elem (zc/csx [A])
          rendered-elem (zcr/render-recursively nil nil elem)]
      (log/debug (zc/print-tree rendered-elem))
      (log/debug "state" (str zc/*updater*))
      (is (= "hello world" (get-in rendered-elem [:props :children 0 :props :children 0]))))))

(deftest should-properly-re-render-on-state-change
  (binding [zc/*updater* (zc/empty-state)]
    (let [A (zc/create-react-class {
               :display-name "A"
               :get-initial-state (fn [] {:value "hello world"})
               :render (fn [this]
                         (let [{:keys [value]} (zc/state this)]
                           (log/warn "render fn. state" (zc/state this))
                           (zc/csx [:div {} [value]])))})
          elem (zc/csx [A])
          rendered-elem (zcr/render-recursively nil nil elem)]
      (log/debug (zc/print-tree rendered-elem))
      (zc/enqueue-set-state! zc/*updater* elem {:value "hola mundo"} nil)
      (zc/update-state! zc/*updater*)
      (let [re-rendered-elem (zcr/render-recursively rendered-elem nil elem)]
        (is (= "hola mundo" (get-in re-rendered-elem [:props :children 0 :props :children 0])))))))

(deftest should-batch-state-changes
  (binding [zc/*updater* (zc/empty-state)]
    (let [A (zc/create-react-class {
               :display-name "A"
               :get-initial-state (fn [] {:value "hello world"})
               :render (fn [this]
                         (let [{:keys [value]} (zc/state this)]
                           (log/warn "render fn. state" (zc/state this))
                           (zc/csx [:div {} [value]])))})
          elem (zc/csx [A])
          rendered-elem (zcr/render-recursively nil nil elem)]
      (log/debug (zc/print-tree rendered-elem))
      (zc/enqueue-set-state! zc/*updater* elem {:value "hola mundo"} nil)
      (zc/enqueue-set-state! zc/*updater* elem {:value "ciao mondo"} nil)
      (zc/update-state! zc/*updater*)
      (let [re-rendered-elem (zcr/render-recursively rendered-elem nil elem)]
        (is (= "ciao mondo" (get-in re-rendered-elem [:props :children 0 :props :children 0])))))))


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
