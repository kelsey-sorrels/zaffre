(ns zaffre.text-test
  (:require
    [zaffre.text :as ztext]
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
    [:text {:style {}} ["abc"]]
    [:text {} [[:text {} "abc"]]]
    ;; cascade multiple levels
    [:text {:style {:fg :red :bg :blue}} [
      [:text {} [
        [:text {} ["abc"]]]]]]
    [:text {:fg :red :bg :blue} [
      [:text {:fg :red :bg :blue} [
        [:text {:fg :red :bg :blue} [
          [:text {:fg :red :bg :blue} "abc"]]]]]]]
    ;; children should override parent styles
    [:text {:style {:fg :red :bg :blue}} [
      [:text {} [
        [:text {:style {:fg :green :bg :black}} ["abc"]]]]]]
    [:text {:fg :red :bg :blue} [
      [:text {:fg :red :bg :blue} [
        [:text {:fg :green :bg :black} [
          [:text {:fg :green :bg :black} "abc"]]]]]]]))

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
