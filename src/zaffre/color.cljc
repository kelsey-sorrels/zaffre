(ns zaffre.color
  (:require [tinter.core :as tinter]))

;; RBG color definitions.
;; It's easier to use names than numbers.
(def color-to-rgb-map
  {:brown       [139 69 19]
   :dark-brown  [70 44 9]
   :ship-brown  [67 51 30]
   :ship-light-brown  [131 88 32]
   :ship-dark-brown  [33 25 15]
   ;;:black       [0 0 0]
   :black       [6 6 11]
   :white       [255 255 255]
   :gray        [128 128 128]
   :light-gray  [64 64 64]
   :dark-gray   [192 192 192]
   :red         [190 38 51];(vec (tinter/hex-str-to-dec "D31C00"))
   :dark-red    [110 18 21];(vec (tinter/hex-str-to-dec "D31C00"))
   :orange      [235 137 49];(vec (tinter/hex-str-to-dec "D36C00"))
   :yellow      [247 226 107];(vec (tinter/hex-str-to-dec "D3B100"))
   ;:highlight   [229 165 8];(vec (tinter/hex-str-to-dec "D3B100"))
   :highlight   (mapv (partial * 0.8) [229 155 8]);(vec (tinter/hex-str-to-dec "D3B100"))
   :background  [6 8 12]
   :sea-foam    [144 213 184]
   :light-green [163 206 39]
   :green       [68 137 26];(vec (tinter/hex-str-to-dec "81D300"))
   :dark-green  (vec (tinter/hex-str-to-dec "406900"))
   :moss-green  [1 140 1]
   :temple-beige [191 171 143]
   :blue-green  [55 148 110];(vec (tinter/hex-str-to-dec "19B4D7"))
   :blue        [0 87 132];(vec (tinter/hex-str-to-dec "00ACD3"))
   :light-blue  [203 219 252];(vec (tinter/hex-str-to-dec "19B4D7"))
   :dark-blue   [0 63 116]
   :purple      (vec (tinter/hex-str-to-dec "8500D3"))
   :fushia      (vec (tinter/hex-str-to-dec "D30094"))
   :light-brown (vec (tinter/hex-str-to-dec "D8C474"))
   :beige       (vec (tinter/hex-str-to-dec "C8B464"))
   :dark-beige  (vec (tinter/hex-str-to-dec "7C612D"))})

(defn limit-color
  ([v]
    (limit-color 0 v 255))
  ([min-v v max-v]
    (min (max min-v v) max-v)))

(defn limit-rgb
  [color limit]
  (mapv (fn [c min-v]
          (limit-color min-v c 255))
        color
        limit))

(defn rgb->mono
  [[r g b]]
  (let [avg (bit-shift-right (+ (max r g b) (min r g b)) 1)]
   (limit-rgb [avg avg avg] [6 6 11])))

(defn color->rgb
  [color]
  {:post [(vector? %)
          (= (count %) 3)
          (every? number? %)]}
  (get color-to-rgb-map color color))


(defn darken-rgb
  ([rgb]
  (mapv #(int (/ % 10)) rgb))
  ([rgb d]
  (assert (vector? rgb) (str "rgb not a vector"))
  (assert (number? (nth rgb 0)) (str (nth rgb 0) " not a number"))
  (assert (number? (nth rgb 1)) (str (nth rgb 1) " not a number"))
  (assert (number? (nth rgb 2)) (str (nth rgb 2) " not a number"))
  (mapv (fn [v min-v] (int (limit-color min-v (* v d) 255))) rgb [5 5 7])))

(defn add-rgb
  [rgb1 rgb2]
  {:post [(vector? %)]}
  (mapv (fn [v1 v2] (limit-color 0 (+ v1 v2) 255)) rgb1 rgb2))

(defn night-tint
  [[r g b] d]
  (if (> d 4)
    [r g b]
    [(/ r 4) (/ g 3) (/ (max r g b) 2)]))

