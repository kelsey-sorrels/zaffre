(ns zaffre.animation.wrapper-test
  (:require
    [zaffre.animation.wrapper :as zaw]
    [clojure.test :refer :all]))


(deftest merge-events-test
  (are [in out] (= (apply zaw/merge-events in) out)
    ;; base cases
    [[]] []
    (list '()) (list)
    [[[0 nil]]] [[0 nil]]
    ;; sinle event stream
    [[[0 :a] [5 :b] [10 :c]]] [[0 :a] [5 :b] [10 :c]]
    [[[5 :a] [5 :b] [10 :c]]] [[5 :a] [5 :b] [10 :c]]
    ;; multiple event streams
    [[] []]    []
    [[[0 :a]]
     [[1 :b]]] [[0 :a] [1 :b]]

    ;; merge two event seqs
    [[[0 :a]]
        [[1 :b] [2 :c]]]   [[0 :a] [1 :b] [2 :c]]

    [    [[1 :b]]
      [[0 :a] [2 :c]]]     [[0 :a] [1 :b] [1 :c]]

    [[[0 :a] [2 :c]]
        [[1 :b]]]          [[0 :a] [1 :b] [1 :c]]

    [[[5 :a]]
     (zaw/delay-events 5 [[5 :b]])] [[5 :a] [5 :b]]
  ))

(deftest merge-infinite-events-test
  (are [in n out] (= (take n (apply zaw/merge-events in)) out)
    ;; sinle event stream
    [(repeat [1 :a])]  3 [[1 :a] [1 :a] [1 :a]]
    ;; multiple event streams

    ;; merge two event seqs
    [(cons [1 :a] (repeat [2 :a]))
     (repeat [2 :b])]               5 [[1 :a] [1 :b] [1 :a] [1 :b] [1 :a]]

    ;; merge three event seqs
    [(cons [1 :a] (repeat [3 :a]))
     (cons [2 :b] (repeat [3 :b]))
     (repeat [3 :c])]               5 [[1 :a] [1 :b] [1 :c] [1 :a] [1 :b]]
  ))


