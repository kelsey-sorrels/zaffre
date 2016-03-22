(ns zaffre.font
  (:require [zaffre.util :as zutil]
            [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (java.io File)
           (java.awt Canvas
                     Color
                     Font
                     FontMetrics
                     Graphics
                     Graphics2D
                     RenderingHints)
           (java.awt.image BufferedImage DataBufferByte)
           (javax.imageio ImageIO)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn- make-font
  [name-or-path style size]
  (let [font-file ^File (clojure.java.io/as-file name-or-path)]
    (if (.exists font-file)
      ;; Load font from file
      (do
        (log/info "Loading font from file" name-or-path)
        (.deriveFont (Font/createFont Font/TRUETYPE_FONT font-file) (int style) (float size)))
      ;; Load font from font registry
      (do
        (log/info "Loading font by name")
        (Font. name-or-path style size)))))

(def cjk-blocks
  (set
    (concat (range 0x2E80 0x2EFF)
            (range 0x3000 0x9FFF)
            (range 0xAC00 0xD7AF)
            (range 0xF900 0xFAFF))))


;; A sequence of [character underline?]
(defn- displayable-characters [^Font font]
  (let [chars (map char (filter (fn [c] (and (.canDisplay font (char c))
                                             (not (contains? cjk-blocks c))))
                                (range 0x0000 0xFFFF)))]
    chars))

(defn- characters-per-line [char-width num-characters]
  (int (Math/floor (/ (Math/pow 2 11) char-width))))

;; A sequence of [\character col row] where [col row] is the column,row in the character atlas.
(defn character-idxs
  [char-width characters]
  (let [character-matrix (partition-all (characters-per-line char-width (count characters)) characters)]
    (log/info "arranging character with" characters-per-line "per line")
    (mapcat concat
    (map-indexed (fn [row line]
                   (map-indexed (fn [col c]
                                  [c col row])
                                line))
                 character-matrix))))


;; Map \character to [col row]
(defn character->col-row
  [character-idxs]
  (reduce (fn [m [c x y]]
            (assoc m c [x y]))
          {}
          character-idxs))


(def cp437-unicode
  (mapv char
    (concat
      [0x0000 0x263A 0x263B 0x2665 0x2666 0x2663 0x2660 0x2022 0x25D8 0x25CB 0x25D9 0x2642 0x2640 0x266A 0x266B 0x263C]
      [0x25BA 0x25C4 0x2195 0x203C 0x00B6 0x00A7 0x25AC 0x21A8 0x2191 0x2193 0x2192 0x2190 0x221F 0x2194 0x25B2 0x25BC]
      (range (int \space) (int \~)) [0x2302]
      [0x00C7 0x00FC 0x00E9 0x00E2 0x00E4 0x00E0 0x00E5 0x00E7 0x00EA 0x00EB 0x00E8 0x00EF 0x00EE 0x00EC 0x00C4 0x00C5]
      [0x00C9 0x00E6 0x00C6 0x00F4 0x00F6 0x00F2 0x00FB 0x00F9 0x00FF 0x00D6 0x00DC 0x00A2 0x00A3 0x00A5 0x20A7 0x0192]
      [0x00E1 0x00ED 0x00F3 0x00FA 0x00F1 0x00D1 0x00AA 0x00BA 0x00BF 0x2310 0x00AC 0x00BD 0x00BC 0x00A1 0x00AB 0x00BB]
      [0x2591 0x2592 0x2593 0x2502 0x2524 0x2561 0x2562 0x2556 0x2555 0x2563 0x2551 0x2557 0x255D 0x255C 0x255B 0x2510]
      [0x2514 0x2534 0x252C 0x251C 0x2500 0x253C 0x255E 0x255F 0x255A 0x2554 0x2569 0x2566 0x2560 0x2550 0x256C 0x2567]
      [0x2568 0x2564 0x2565 0x2559 0x2558 0x2552 0x2553 0x256B 0x256A 0x2518 0x250C 0x2588 0x2584 0x258C 0x2590 0x2580]
      [0x03B1 0x00DF 0x0393 0x03C0 0x03A3 0x03C3 0x00B5 0x03C4 0x03A6 0x0398 0x03A9 0x03B4 0x221E 0x03C6 0x03B5 0x2229]
      [0x2261 0x00B1 0x2265 0x2264 0x2320 0x2321 0x00F7 0x2248 0x00B0 0x2219 0x00B7 0x221A 0x207F 0x00B2 0x25A0 0x00A0])))


(defn- ^BufferedImage convert-type [^BufferedImage buffered-image image-type]
  (let [converted-image    (BufferedImage. (.getWidth buffered-image) (.getHeight buffered-image) image-type)
        converted-graphics (.createGraphics converted-image)]
    (doto converted-graphics
      (.drawImage buffered-image 0 0 nil)
      (.dispose))
    converted-image))

(defn- copy-channel [v channel]
  (let [channel-shift (case channel
                       :alpha 24
                       :blue  16
                       :green  8
                       :red    0)
        c (->
            (bit-shift-left 0xFF channel-shift)
            (bit-and v)
            (unsigned-bit-shift-right channel-shift))]
    (reduce (fn [a n] (bit-or a (bit-shift-left c n))) 0 [24 16 8 0])))

(defn- ^BufferedImage copy-channels! [^BufferedImage buffered-image channel]
  (doseq [x (range (.getWidth buffered-image))
          y (range (.getHeight buffered-image))]
      (.setRGB buffered-image  x y (copy-channel (.getRGB buffered-image x y) channel)))
  buffered-image)

(defn- ^BufferedImage scale-image [^BufferedImage buffered-image scale]
  (let [scaled-image    (BufferedImage. (* scale (.getWidth buffered-image))
                                        (* scale (.getHeight buffered-image))
                                        (.getType buffered-image))
        scaled-graphics (.createGraphics scaled-image)]
    (doto scaled-graphics
      (.scale scale scale)
      (.drawImage buffered-image 0 0 nil)
      (.dispose))
    scaled-image))

(defrecord CP437Font [path-or-url alpha scale])

(defrecord TTFFont [name-or-path size])

(defrecord CompositeFont [fonts])

(defrecord TileSet [path-or-url alpha tile-width tile-height tile-id->col-row])

(defmulti glyph-graphics class)
(defmethod glyph-graphics CP437Font [font]
  (with-open [image-stream (jio/input-stream (get font :path-or-url))]
    (let [characters    (map char cp437-unicode)
          font-image    (->
                          (ImageIO/read image-stream)
                          (convert-type BufferedImage/TYPE_4BYTE_ABGR)
                          (scale-image (get font :scale))
                          (copy-channels! (get font :alpha)))
          width         (zutil/next-pow-2 (.getWidth font-image))
          height        (zutil/next-pow-2 (.getHeight font-image))
          cwidth        (/ (.getWidth font-image) 16)
          cheight       (/ (.getHeight font-image) 16)
          texture-image (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          texture-graphics (.createGraphics texture-image)]

      (log/info "Using cp437 font image" (.getType font-image))
      (doto texture-graphics
        (.setColor (Color. 0 0 0 0))
        (.fillRect 0 0 width height))
      (while (not (.drawImage texture-graphics font-image 0 0 nil))
        (Thread/sleep(100)))
      (.dispose texture-graphics)
      (ImageIO/write font-image "png", (File. "font-texture.png"))
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image
       :character-width cwidth
       :character-height cheight
       :character->col-row (zipmap
                             characters
                             (for [row (range 16)
                                   col (range 16)]
                               [col row]))})))

(defmethod glyph-graphics TTFFont [font]
  ;; Adjust canvas to fit character atlas size
  (let [j-font                     (make-font (get font :name-or-path) Font/PLAIN (get font :size))
        characters                 (displayable-characters j-font)
        font-metrics  ^FontMetrics (.getFontMetrics (Canvas.) j-font)
        char-width                 (.charWidth font-metrics \M)
        char-height                (.getHeight font-metrics)
        _ (log/info "characters per line" (characters-per-line char-width (count characters)))
        ;screen-width               (* columns char-width)
        ;screen-height              (* rows char-height)
        cwidth     2048
        cheight    (zutil/next-pow-2 (* char-height (int (Math/ceil (/ (count characters)
                                                                 (characters-per-line char-width (count characters)))))))
        width      cwidth ;(max cwidth cheight)
        height     cheight ;(max cwidth cheight)
        antialias  true
        char-idxs   (character-idxs char-width characters)]
    (log/info "characters" (count characters) "cwidth" cwidth "cheight" cheight)
    (log/info "glyph image width" width "height" height)
    ;(log/info "characters" (vec characters))
    ;(log/info "character-idxs" (vec (character-idxs characters)))
    (let [texture-image    (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          texture-graphics (.createGraphics texture-image)
          white            (Color. 255 255 255 255)]
      ;; Create and clear graphics
      (doto texture-graphics
        (.setFont j-font)
        (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING (if antialias
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
         ;; set background to black
        (.setColor (Color. 0 0 0 0))
        (.fillRect 0 0 width height))
      ;; Loop through each character, drawing it
      (doseq [[c col row]  char-idxs]
        (let [x ^int (* col char-width)
              y ^int (* (inc row) char-height)
              cx (+ 0 x)
              cy (- y (.getDescent font-metrics))]
          ;(log/info s x y)
        (when (not= c \space)
          ;(println "drawing" s "@" x y "underline?" underline?)
          (doto texture-graphics
            (.setColor white)
            (.setClip cx (+ (- cy char-height) (.getDescent font-metrics)) char-width char-height)
            (.drawString (str c) cx cy)))))
      ;; cleanup texture resource
      (ImageIO/write texture-image "png", (File. "glyph-texture.png"))
      (.dispose texture-graphics)
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image
       :character-width char-width
       :character-height char-height
       :character->col-row (character->col-row char-idxs)})))


(defmethod glyph-graphics CompositeFont [font]
  (let [glyphs (mapv glyph-graphics (get font :fonts))]
    (assert (reduce = (map :character-width glyphs)) (str "Not all character widths equal " (vec (mapv :character-width glyphs))))
    (assert (reduce = (map :character-height glyphs)) (str "Not all character heights equal " (vec (mapv :character-height glyphs))))
    (log/info "Using unified character-width" (-> glyphs first :character-width))
    (log/info "Using unified character-height" (-> glyphs first :character-height))
    ;; Lay fonts out like this
    ;;  +----------+----+
    ;;  |  font 1  |    |
    ;;  |          |    |
    ;;  +----------+----+
    ;;  |  font 2       |
    ;;  |               |
    ;;  |               |
    ;;  +-----+---------+
    ;;  |font3|         |
    ;;  |     |         |
    ;;  +-----+---------+
    ;;
    (let [width              (zutil/next-pow-2 (reduce max 0 (map :font-texture-width glyphs)))
          height             (zutil/next-pow-2 (reduce + 0 (map :font-texture-height glyphs)))
          composite-image    (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          composite-graphics (.createGraphics composite-image)
          character-width    (-> glyphs first :character-width)
          character-height   (-> glyphs first :character-height)]
      (loop [y 0
             character->col-row {}
             glyphs glyphs]
        (if-let [glyph (first glyphs)]
          (let [image ^BufferedImage (get glyph :font-texture-image)]
            (.drawImage composite-graphics image 0 y nil)
            (recur
              (+ y (int (get glyph :font-texture-height)))
              (reduce-kv (fn [m k [col row]]
                           (assoc m k [col (+ row (/ y character-height))]))
                         character->col-row
                         (get glyph :character->col-row))
              (rest glyphs)))
          (do
            (.dispose composite-graphics)
            (ImageIO/write composite-image "png", (File. "composite-texture.png"))
            {:font-texture-width width
             :font-texture-height height
             :font-texture-image composite-image
             :character-width character-width
             :character-height character-height
             :character->col-row character->col-row}))))))

(defmethod glyph-graphics TileSet [font]
  (with-open [image-stream (jio/input-stream (get font :path-or-url))]
    (let [font-image    (->
                          (ImageIO/read image-stream)
                          (convert-type BufferedImage/TYPE_4BYTE_ABGR)
                          (copy-channels! (get font :alpha)))
          width         (zutil/next-pow-2 (.getWidth font-image))
          height        (zutil/next-pow-2 (.getHeight font-image))
          texture-image (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          texture-graphics (.createGraphics texture-image)]
      (doto texture-graphics
        (.setColor (Color. 0 0 0 0))
        (.fillRect 0 0 width height))
      (while (not (.drawImage texture-graphics font-image 0 0 nil))
        (Thread/sleep(100)))
      (.dispose texture-graphics)
      (ImageIO/write font-image "png", (File. "tileset-texture.png"))
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image
       :character-width (get font :tile-width)
       :character-height (get font :tile-height)
       :character->col-row (get font :tile-id->col-row)})))

