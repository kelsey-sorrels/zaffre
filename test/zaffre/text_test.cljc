(ns zaffre.text-test
  (:require
    [zaffre.text :as ztext]
    [cashmere.core-graal :as cm]
    [taoensso.timbre :as log]
    [clojure.inspector :refer :all]
    [clojure.test :refer :all]))

(log/set-level! :warn)

(deftest should-word-wrap
  (are [input expected] (= expected (apply ztext/word-wrap input))
    [5 "Not A"]
    ["Not A"]))

(deftest should-cascade-style
  (are [input expected] (= expected (ztext/cascade-style {} input))
    ;; pass through
    (cm/new-text-instance {:style {}} "abc")
    [:raw-text {} "abc"]
    ;; cascade multiple levels
    (cm/new-instance :text {:style {:fg :red :bg :blue}} [
      (cm/new-instance :text {:style {:fg :red :bg :blue}} [
        (cm/new-text-instance {} "abc")])])
    [:text {:fg :red :bg :blue} [
      [:text {:fg :red :bg :blue} [
        [:raw-text {:fg :red :bg :blue} "abc"]]]]]
    ;; children should override parent styles
    (cm/new-instance :text {:style {:fg :red :bg :blue}} [
      (cm/new-instance :text {} [
        (cm/new-text-instance {:style {:fg :green :bg :black}} "abc")])])
    [:text {:fg :red :bg :blue} [
      [:text {:fg :red :bg :blue} [
        [:raw-text {:fg :green :bg :black} "abc"]]]]]))

(deftest should-seq-through-tree
  (are [input expected] (= expected (ztext/text-tree-seq input))
    ;; pass through
    [:text {} [
      [:text {} "abc"]]]
    [[:text {} [
      [:text {} "abc"]]]
     [:text {} "abc"]]
))

(deftest should-seq-through-leaves
  (are [input expected] (= expected (-> input ztext/text-tree-seq ztext/filter-leaves))
    ;; seq multiple levels
    [:text {} [
      [:text {} [
        [:text {} "abc"]]]]]
    [[:text {} "abc"]]
    ;; should work with multiple leaf scenarios
    [:text {} [
      [:text {} "abc"]
      [:text {} "def"]
      [:text {} [
        [:text {} "hij"]
        [:text {} [
          [:text {} "klm"]
          [:text {} "nop"]]]
        [:text {} "qrs"]]]
      [:text {} "tuv"]
      [:text {} "wxy"]]]
    [[:text {} "abc"]
     [:text {} "def"]
     [:text {} "hij"]
     [:text {} "klm"]
     [:text {} "nop"]
     [:text {} "qrs"]
     [:text {} "tuv"]
     [:text {} "wxy"]]))

(deftest should-wrap
  (are [input expected] (= expected (ztext/word-wrap 26 input))
    ;; pass through
    "You begin crafting a weapon. You'll need to start with an item."
    ["You begin crafting a "
     "weapon. You'll need to "
     "start with an item."]
))

(deftest should-word-wrap-text-tree
  (are [input expected] (= expected (ztext/word-wrap-text-tree 26 1 input))
    (cm/new-instance :text {:style {:fg :red :bg :blue}} [
      (cm/new-instance :text {} [
        (cm/new-text-instance {:style {:fg :green :bg :black}} "_______")])])
    [[["_______" {:fg :green, :bg :black} 7]]]))
    

(deftest should-get-text
  (are [input expected] (= expected (ztext/text input))
    (cm/new-instance :text {:style {:fg :red :bg :blue}} [
      (cm/new-instance :text {} [
        (cm/new-text-instance {:style {:fg :green :bg :black}} "_______")])])
    "_______"))
