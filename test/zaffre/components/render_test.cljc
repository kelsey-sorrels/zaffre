(ns zaffre.components.render-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.render :as zcr]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

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

(zc/def-component red-text [this]
  (let [{:keys [children]} (zc/props this)]
    (println "red-text render" children)
    (zc/csx [:text {:style {:color :red}} children])))

(zc/def-component hello-world-label-csx [this]
  (let [{:keys [value]} (zc/props this)]
    (zc/csx [red-text {} [(str "Hello " value)]])))

(zc/def-component debug-hello-world-label []
  ((zc/debug-lifecycle-handlers) hello-world-label-csx))

#_(deftest render-test-0
  (are [in out] (= out (zcr/render-recursively nil nil in))
    (zc/csx [hello-world-label-csx {:value "world"}])
    (zc/create-element
      :text
      {:display-name ":text"
       :style {:color :red}}
      ["Hello world"])))

(deftest render-test-1
  (let [element (zc/csx [hello-world-label-csx {:value "world"}])
        rendered-element (zcr/render-recursively nil nil element)
        re-rendered-element (zcr/render-recursively rendered-element nil element)]
    (inspect-tree rendered-element)
    (inspect-tree re-rendered-element)
    (is (= re-rendered-element rendered-element))))


