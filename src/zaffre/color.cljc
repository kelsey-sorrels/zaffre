(ns zaffre.color
  (:require [tinter.core :as tinter]))

;; construct colors as ints
;; Little-endian order due to that being the native order for bytebuffers
(defmacro const-color
  ([r g b]
    `(color ~r ~g ~b 255))
  ([r g b a]
    (bit-or
      (bit-shift-left (bit-and 0xff r) (int 0))
      (bit-shift-left (bit-and 0xff g) (int 8))
      (bit-shift-left (bit-and 0xff b) (int 16))
      (bit-shift-left (bit-and 0xff a) (int 24)))))
  
(defn color 
  ([r g b]
    (color r g b 255))
  ([r g b a]
    (let [a (int (or a 255))]
      (bit-or
        (bit-shift-left (bit-and 0xff r) (int 0))
        (bit-shift-left (bit-and 0xff g) (int 8))
        (bit-shift-left (bit-and 0xff b) (int 16))
        (bit-shift-left (bit-and 0xff a) (int 24)))))
  ([[r g b a]]
    (color r g b a)))

(defn red [color]
  (let [color (unchecked-int color)]
    (bit-and 0x000000ff color)))

(defn green [color]
  (let [color (unchecked-int color)]
    (bit-shift-right (bit-and 0x0000ff00 color) (int 8))))

(defn blue [color]
  (let [color (unchecked-int color)]
    (bit-shift-right (bit-and 0x00ff0000 color) (int 16))))

(defn alpha [color]
  (let [color (unchecked-int color)]
    (bit-shift-right (bit-and 0xff000000 color) (int 24))))

(defn with-alpha [color alpha]
  (let [color (unchecked-int color)]
    (bit-or (bit-and 0x00ffffff color)
            (bit-shift-left (bit-and 0xff alpha) (int 24)))))

