(ns zaffre.components.render-test
  (:require
    [zaffre.components :as zc]
    [zaffre.components.render :as zcr]
    [zaffre.text :as ztext]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

#_(deftest wrap-lines-test
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

#_(deftest render-test-1
  (binding [zc/*updater* (zc/empty-state)]
    (let [element (zc/csx [hello-world-label-csx {:value "world"}])
          rendered-element (zcr/render-recursively nil nil element)
          re-rendered-element (zcr/render-recursively rendered-element nil element)]
      (is (= re-rendered-element rendered-element)))))

