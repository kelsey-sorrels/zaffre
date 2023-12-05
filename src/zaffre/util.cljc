(ns zaffre.util
  (:require [zaffre.terminal :as zat]
            clojure.set)
  (:import (zaffre.terminal Terminal)))

(defn next-pow-2 [v]
  (int (Math/pow 2 (Math/ceil (/ (Math/log v) (Math/log 2))))))

(defn mk-string
  ([x y string]
   (mk-string (int (Math/ceil x)) (int (Math/ceil y)) string [255 255 255] [0 0 0] #{}))
  ([x y string fg bg]
   (mk-string (int (Math/ceil x)) (int (Math/ceil y)) string fg bg #{}))
  ([x y string fg bg styles]
   {:pre [(clojure.set/superset? #{:underline :bold} styles)]}
   (map-indexed (fn [i c] {:c  c
                           :fg fg
                           :bg bg
                           :x  (+ x i)
                           :y  y})
                string)))
(defn put-string
  ([^Terminal screen layer-id x y string]
   (put-string screen layer-id (int (Math/ceil x)) (int (Math/ceil y)) string [255 255 255] [0 0 0] #{}))
  ([^Terminal screen layer-id x y string fg bg]
   (put-string screen layer-id (int (Math/ceil x)) (int (Math/ceil y)) string fg bg #{}))
  ([^Terminal screen layer-id x y string fg bg styles]
   (zat/put-chars! screen layer-id (mk-string x y string fg bg styles))))


;; Memoized function that returns the points between
;; `[x1 y1]` and `[x2 y2]`
(def line-segment (memoize
  (fn
   [start end]
   (if (= start end)
      []
      (let [[x1 y1] start
            [x2 y2] end
            xdiff (- x2 x1)
            ydiff (- y2 y1)
            maxdiff (max (Math/abs xdiff) (Math/abs ydiff))
            dx (/ xdiff maxdiff)
            dy (/ ydiff maxdiff)]
        (map (fn [i] [(Math/round (double (+ x1 (* i dx)))) (Math/round (double (+ y1 (* i dy))))])
            (range (inc maxdiff))))))))

;; A fast version of `line-segment`. Internally, shift the values so that
;; `[x1 y1]` equals `[0 0]`, call `line-segment` and then shift everything back.
;; It's fast because `(line-segment-fast [0 0] [5 5])` is effectively the same
;; as `(line-segment [2 2] [7 7])` which plays nicely with memoization.
(def line-segment-fast (memoize
  (fn [start end]
    "(line-segment-fast [1 1] [5 4])"
    (let [[ox oy] start
          [dx dy] end]
      (map (fn [[x y]] [(+ ox x) (+ oy y)])
          (line-segment [0 0] [(- dx ox) (- dy oy)]))))))

;; A fast version of `line-segment`. Internally, shift the values so that
;; `[x1 y1]` equals `[0 0]`, call `line-segment` and then shift everything back.
;; It's fast because `(line-segment-fast [0 0] [5 5])` is effectively the same
;; as `(line-segment [2 2] [7 7])` which plays nicely with memoization.
(def line-segment-fast-without-endpoints (memoize
  (fn [start end]
    "(line-segment-fast [1 1] [5 4])"
    (let [[ox oy] start
          [dx dy] end]
      (rest
        (butlast
          (map (fn [[x y]] [(+ ox x) (+ oy y)])
               (line-segment [0 0] [(- dx ox) (- dy oy)]))))))))

(defn dot [u v]
  (reduce + 0 (map * u v)))

(defn norm [v]
  (Math/sqrt (reduce + 0 (map (fn [x] (* x x)) v))))

(defn unit [v]
  (let [l (norm v)]
    (map #(/ % l) v)))

(defn light-intensity [dx dy dz]
  (let [rl  [dx dy dz]
        d   (norm rl) ; distance from [x y] to light
        i (min 1.0 (/ (* 1.0 (dot (unit [0 0 1]) (unit rl))) (* d d)))]
    i))
 
