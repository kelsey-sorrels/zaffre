(ns zaffre.font
  (:require [zaffre.util :as zutil]
            [zaffre.imageutil :as zimg]
            [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (zaffre.imageutil Image)
           (java.io File)
           (java.nio ByteBuffer)
           (org.lwjgl BufferUtils)
           (org.lwjgl.stb STBTruetype STBTTFontinfo)
           (org.apache.commons.io IOUtils)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn- make-font
  [path]
  (let [font-file ^File (clojure.java.io/as-file path)]
    (if (.exists font-file)
      ;; Load font from file
      (do
        (log/info "Loading font from file" path)
        (let [info (STBTTFontinfo/calloc)]
          (when (zero?
                  (STBTruetype/stbtt_InitFont info (-> font-file
                                                     jio/input-stream
                                                     IOUtils/toByteArray
                                                     ByteBuffer/wrap)))
            (throw (RuntimeException. "Error loading font")))
          info))
      (throw (RuntimeException. "Font does not exit")))))

(def cjk-blocks
  (set
    (concat (range 0x2E80 0x2EFF)
            (range 0x3000 0x9FFF)
            (range 0xAC00 0xD7AF)
            (range 0xF900 0xFAFF))))


;; A sequence of [character underline?]
(defn- displayable-characters [^STBTTFontinfo font]
  (let [chars (map char
                   (filter (fn [c]
                             (and (pos? (STBTruetype/stbtt_FindGlyphIndex font (int c)))
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


(defn compact [^Image image tile-width tile-height margin]
  (if (pos? margin)
    (let [cols                   (quot (zimg/width image) (+ tile-width margin))
          rows                   (quot (zimg/height image) (+ tile-height margin))
          compact-image (zimg/image (* cols tile-width) (* rows tile-height))]
      (log/info tile-width tile-height)
      (log/info "Copying cols" cols "rows" rows)
      (doseq [row (range rows)
              col (range cols)
              :let [dx1 (* col tile-width)
                    dy1 (* row tile-height)
                    dx2 (+ dx1 tile-width)
                    dy2 (+ dy1 tile-height)
                    ;; source
                    sx1 (* col (+ tile-width margin))
                    sy1 (* row (+ tile-height margin))
                    sx2 (+ sx1 tile-width)
                    sy2 (+ sy1 tile-height)]]
        (log/info "Copying from" sx1 sy1 sx2 sy2 "to" dx1 dy1 dx2 dy2)
        (log/info "row" row "tile-height" tile-height)
        (zimg/copy-sub-image compact-image
            image
            ;; destination
            dx1 dy1
            ;; source
            sx1 sy1 sx2 sy2))
      ;(ImageIO/write compact-buffered-image "png", (File. "compact-texture.png"))
      compact-image)
    image))

(defn- advance-width [^STBTTFontinfo font-info codepoint]
  (let [advance-width (BufferUtils/createIntBuffer 1)
        left-side-bearing (BufferUtils/createIntBuffer 1)]
    (STBTruetype/stbtt_GetCodepointHMetrics font-info (int codepoint) advance-width left-side-bearing)
    (.get advance-width)))

(defrecord CP437Font [path-or-url alpha scale transparent])

(defrecord TTFFont [name-or-path size transparent])

(defrecord CompositeFont [fonts])

(defrecord TileSet [path-or-url alpha tile-width tile-height margin tile-id->col-row tile-id->transparent])

(defmulti glyph-graphics class)

(defn glyph-graphics? [m]
  (every?
      #{:font-texture-width
       :font-texture-height
       :font-texture-image
       :character-width
       :character-height
       :character->col-row
       :character->transparent}
      (keys m)))

(defmethod glyph-graphics CP437Font [font]
  {:post [(glyph-graphics? %)]}
  (let [img (-> (get font :path-or-url)
              zimg/load-image
              (zimg/mode :rgba))]
    (log/info "img" img)
    (let [characters    (map char cp437-unicode)
          font-image    (-> img
                          (zimg/copy-channels (get font :alpha))
                          (cond->
                          (> (get font :scale) 1)
                            (zimg/scale (get font :scale))))
          width         (zutil/next-pow-2 (zimg/width font-image))
          height        (zutil/next-pow-2 (zimg/height font-image))
          cwidth        (/ (zimg/width font-image) 16)
          cheight       (/ (zimg/height font-image) 16)
          _ (log/info "font-image" font-image)
          texture-image (zimg/resize font-image width height)
          character->col-row (zipmap
                             characters
                             (for [row (range 16)
                                   col (range 16)]
                               [col row]))]

      (log/info "Using cp437 font image")
      (log/info "texture-image" texture-image)
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image
       :character-width cwidth
       :character-height cheight
       :character->col-row character->col-row
       :character->transparent (zipmap characters (repeat (get font :transparent)))})))


(defn- char-image [font-info size c]
  (let [scale       (STBTruetype/stbtt_ScaleForPixelHeight
                      font-info
                      size)
        char-width  (* (advance-width font-info (int \M)) scale)
        char-height size
        img         (zimg/image char-width char-height 1)]
    ;; draw greyscale font
    (STBTruetype/stbtt_MakeCodepointBitmap
      font-info
      (get img :byte-buffer)
      (zimg/width img)
      (zimg/height img)
      0
      scale
      scale
      (int c))))
  
(defmethod glyph-graphics TTFFont [font]
  #_{:post [(glyph-graphics? %)]}
  ;; Adjust canvas to fit character atlas size
  (let [font-info                  (make-font (get font :name-or-path))
        scale                      (STBTruetype/stbtt_ScaleForPixelHeight
                                     font-info
                                     (get font :size))
        characters                 (displayable-characters font-info)
        char-width                 (* (advance-width font-info (int \M)) scale)
        char-height                (get font :size)
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
    (let [texture-image    (zimg/image width height)]
      ;; Loop through each character, drawing it
      (doseq [[c col row]  char-idxs]
        (let [x  (* col char-width)
              y  (* (inc row) char-height)
              y  (* row char-height)
              cx (+ 0 x)
              cy (- y 0 #_(.getDescent font-metrics))]
          ;(log/info s x y)
        (when (not= c \space)
          ;(println "drawing" s "@" x y "underline?" underline?)
          (zimg/draw-image texture-image
                           (char-image font-info (get font :size) c)
                           x
                           y))))
      ;; cleanup texture resource
      (zimg/write-png texture-image "glyph-texture.png")
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image
       :character-width char-width
       :character-height char-height
       :character->col-row (character->col-row char-idxs)
       :character->transparent (zipmap characters (repeat (get font :transparent)))})))


(defmethod glyph-graphics CompositeFont [font]
  {:post [(glyph-graphics? %)]}
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
          composite-image    (zimg/image width height)
          character-width    (-> glyphs first :character-width)
          character-height   (-> glyphs first :character-height)]
      (loop [y 0
             character->col-row {}
             character->transparent {}
             glyphs glyphs]
        (if-let [glyph (first glyphs)]
          (let [image (get glyph :font-texture-image)]
            (log/info "drawing to composite-image" 0 y)
            (zimg/draw-image composite-image image 0 y)
            ;;(log/info "merging" (get glyph :character->transparent))
            (recur
              (+ y (int (get glyph :font-texture-height)))
              (reduce-kv (fn [m k [col row]]
                           (assoc m k [col (+ row (quot y character-height))]))
                         character->col-row
                         (get glyph :character->col-row))
              (merge character->transparent
                     (get glyph :character->transparent))
              (rest glyphs)))
          (do
            (zimg/write-png composite-image "composite-texture.png")
            {:font-texture-width width
             :font-texture-height height
             :font-texture-image composite-image
             :character-width character-width
             :character-height character-height
             :character->col-row character->col-row
             :character->transparent character->transparent}))))))

(defmethod glyph-graphics TileSet [font]
  {:post [(glyph-graphics? %)]}
  (let [font-image (-> (zimg/load-image (get font :path-or-url))
                     (compact (get font :tile-width)
                              (get font :tile-height)
                              (get font :margin))
                     (as-> font-image
                       (let [alpha (get font :alpha)]
                         (if-not (= alpha :alpha)
                           (zimg/copy-channels font-image (get font :alpha))
                           font-image))))
        width         (zutil/next-pow-2 (zimg/width font-image))
        height        (zutil/next-pow-2 (zimg/height font-image))
        texture-image (zimg/resize font-image width height)]
    (zimg/write-png font-image "tileset-texture.png")
    {:font-texture-width width
     :font-texture-height height
     :font-texture-image texture-image
     :character-width (get font :tile-width)
     :character-height (get font :tile-height)
     :character->col-row (get font :tile-id->col-row)
     :character->transparent (get font :tile-id->transparent)}))


(defn construct [font] (glyph-graphics font))
