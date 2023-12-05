(ns zaffre.font
  (:require [zaffre.util :as zutil]
            [zaffre.imageutil :as zimg]
            [zaffre.lwjglutil :as zlwjgl]
            [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (zaffre.imageutil Image)
           (java.io File IOException)
           (java.net URL MalformedURLException)
           (java.nio ByteBuffer)
           (java.nio.file Files Path)
           (org.lwjgl BufferUtils)
           (org.lwjgl.system MemoryStack)
           (org.lwjgl.stb STBTruetype STBTTFontinfo)
           (org.apache.commons.io IOUtils)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro defn-ms [name [memory-stack & args] & body]
  `(defn ~name [~memory-stack ~@args]
     (.push ~memory-stack)
     (try
       ~@body
       (finally
         (.pop ~memory-stack)))))

(def calls (atom []))
;(add-watch calls :persist
;  (fn [_ _ _ calls]
;    (spit "calls.log" calls)))
    
(defn- byte-buffer->str [^ByteBuffer buffer]
  (when buffer
    (loop [s ""]
      (if (pos? (.remaining buffer))
         (recur (str s (char (.get buffer))))
         s))))

(defn font-family [font-info]
  (byte-buffer->str (STBTruetype/stbtt_GetFontNameString font-info 1 0 0 1)))

(defn font-name [font-info]
  (byte-buffer->str (STBTruetype/stbtt_GetFontNameString font-info 1 0 0 4)))

(defn- linux-font-paths []
  (let [paths [(str (System/getProperty "user.home") "/.fonts")
               "/usr/share/fonts/truetype"
               "/usr/share/fonts/TTF"]]
    (filter (fn [path]
              (let [file (clojure.java.io/as-file path)]
                (and (.exists file)
                     (.isDirectory file)
                     (.canRead file))))
            paths)))

(defn- macosx-font-paths []
  [(str (System/getProperty "user.home") File/separator ".fonts")
   "/Library/Fonts"
   "/System/Library/Fonts"])

(defn- windows-font-paths []
  [(str (System/getenv "WINDIR") "\\" "Fonts")])

(defn- files [path]
  (file-seq (jio/file path)))

(defn- font-paths []
  (let [extensions #{"ttf" "TTF"}
        font-paths (case (zlwjgl/platform)
                     :linux (linux-font-paths)
                     :macosx (macosx-font-paths)
                     :window (windows-font-paths))]
    (filter (fn [^Path path]
              (.exists (File. (str path))))
      (remove nil?
        (for [path font-paths
              ^File file (files path)
              :when (some (fn [extension] (.endsWith (.getAbsolutePath file)  extension))
                          extensions)]
          (let [^Path file-path (.toPath file)]
            (if (Files/isSymbolicLink file-path)
              (let [^Path link (Files/readSymbolicLink file-path)]
                (try 
                  (Files/readSymbolicLink link)
                  (catch IOException e
                    nil)))
              (.toPath file))))))))

(declare make-font)
(declare font-data)

(def system-fonts-map
  (delay
    (into {}
          (mapcat identity
            (for [path (font-paths)]
              (try
                #_(log/info "type path" (type path))
                (let [file        (.toFile path)
                      font-info   (make-font (font-data file))
                      font-name   (font-name font-info)
                      font-family (font-family font-info)]
                  [[font-name file]
                   [font-family file]])
                (catch Exception e
                  (log/error e "Error loading" path ))))))))

(defn system-fonts []
  @system-fonts-map)

(defn- as-font-path
  [x]
  (if (= File (type x))
    x
    (try
      (URL. x)
      (catch MalformedURLException err
          (let [file (File. x)]
            (if (.exists file)
              file
              (get (system-fonts) x)))))))
  

(defn font-data
  [x]
  (if-let [font-path (as-font-path x)]
    ;; Load font from file
    (let [info         (STBTTFontinfo/calloc)
          buffer       (-> font-path
                         jio/input-stream
                         IOUtils/toByteArray
                         ByteBuffer/wrap)
         direct-buffer (BufferUtils/createByteBuffer (.limit buffer))]
      (doto direct-buffer
        (.put buffer)
        (.flip)))
    (throw (RuntimeException. (str "Font " x " does not exist")))))
  
(defn make-font
  [^ByteBuffer font-data]
  (let [info (STBTTFontinfo/calloc)
        result ^boolean (STBTruetype/stbtt_InitFont info font-data)]
    (if-not result
      (throw (RuntimeException. "Error loading font"))
      info)))

(defn cjk-codepoint?
  [codepoint]
    (or (<= 0x2E80 codepoint 0x2EFF)
        (<= 0x3000 codepoint 0x9FFF)
        (<= 0xAC00 codepoint 0xD7AF)
        (<= 0xF900 codepoint 0xFAFF)))


(defn- displayable-characters [^STBTTFontinfo font]
  "Returns a map from codepoint to glyph index"
  (into {}
    (reduce (fn [m codepoint]
             (let [glyph-index (STBTruetype/stbtt_FindGlyphIndex font (int codepoint))]
               (if  (and (pos? glyph-index)
                         (not (cjk-codepoint? codepoint)))
                 (assoc m codepoint glyph-index)
                 m)))
          {}
          (range 0x0000 0xFFFF))))
  

(defn- characters-per-line [char-width num-characters]
  (int (Math/floor (/ (Math/pow 2 11) char-width))))

;; A sequence of [\character col row] where [col row] is the column,row in the character atlas.
(defn character-idxs
  [char-width codepoint->glyph-index]
  (let [character-matrix (partition-all (characters-per-line char-width (count codepoint->glyph-index)) codepoint->glyph-index)]
    #_(log/info "arranging character with" characters-per-line "per line")
    (mapcat concat
    (map-indexed (fn [row line]
                   (map-indexed (fn [col [codepoint glyph-index]]
                                  [codepoint glyph-index col row])
                                line))
                 character-matrix))))


;; Map \character to [col row]
(defn character->col-row
  [character-idxs]
  (reduce (fn [m [codepoint _ col row]]
            (assoc m (char codepoint) [col row]))
          {}
          character-idxs))


(def cp437-unicode
  (mapv char
    (concat
      [0x0000 0x263A 0x263B 0x2665 0x2666 0x2663 0x2660 0x2022 0x25D8 0x25CB 0x25D9 0x2642 0x2640 0x266A 0x266B 0x263C]
      [0x25BA 0x25C4 0x2195 0x203C 0x00B6 0x00A7 0x25AC 0x21A8 0x2191 0x2193 0x2192 0x2190 0x221F 0x2194 0x25B2 0x25BC]
      (range (int \space) (inc (int \~))) [0x2302]
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

(defn-ms hmetrics [^MemoryStack ms ^STBTTFontinfo font-info glyph-index]
  (let [advance-width     (.mallocInt ms 1)
        left-side-bearing (.mallocInt ms 1)]
    (STBTruetype/stbtt_GetGlyphHMetrics font-info (int glyph-index) advance-width left-side-bearing)
    [(.get advance-width) (.get left-side-bearing)]))

(defn-ms vmetrics [^MemoryStack ms ^STBTTFontinfo font-info]
  (let [ascent   (.mallocInt ms 1)
        descent  (.mallocInt ms 1)
        line-gap (.mallocInt ms 1)]
    (STBTruetype/stbtt_GetFontVMetrics font-info ascent descent line-gap)
    [(.get ascent) (.get descent) (.get line-gap)]))

(defn-ms glyph-bitmap-box [^MemoryStack ms ^STBTTFontinfo font-info scale glyph-index]
  (let [ix0 (.mallocInt ms 1)
        iy0 (.mallocInt ms 1)
        ix1 (.mallocInt ms 1)
        iy1 (.mallocInt ms 1)]
    (STBTruetype/stbtt_GetGlyphBitmapBox
      font-info
      (int glyph-index)
      (float scale)
      (float scale)
      ix0
      iy0
      ix1
      iy1)
    [(.get ix0) (.get iy0) (.get ix1) (.get iy1)]))


(defn- char-image [ms font-info char-width char-height ascent scale codepoint glyph-index]
  (let [;scale 0.015625
        [x0 y0 x1 y1]         (glyph-bitmap-box ms font-info scale glyph-index)
        baseline              (* ascent scale)
        [_ left-side-bearing] (map (partial * scale) (hmetrics ms font-info glyph-index))
        y                     (int (Math/ceil (+ baseline y0)))
        img                   (zimg/image char-width char-height 1)
        chr-img               (zimg/image char-width char-height 1)]
    ;(log/info "char-width" (char codepoint) "(" (int codepoint) ")" char-width char-height
    ;        "ascent" ascent "scale" scale "baseline" baseline
    ;        "left-side-bearing" left-side-bearing "y" y)
    ;(log/info "x0" x0 "y0" y0 "x1" x1 "y1" y1)
    ;; draw greyscale font
    (STBTruetype/stbtt_MakeGlyphBitmapSubpixel
      font-info
      (get img :byte-buffer)
      char-width 
      char-height
      char-width
      scale
      scale
      0.0
      0.0
      (int glyph-index))
    (zimg/draw-image chr-img img left-side-bearing y)
    (zimg/write-png chr-img (str "char-image/" (int codepoint) ".png"))
    (zimg/write-png img (str "char-image/" (int codepoint) "-raw.png"))
    ;; convert to rgba
    (zimg/mode chr-img :rgba)))
  
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

(defprotocol GlyphGraphics
  (glyph-graphics [this]))

(defrecord CP437Font [path-or-url alpha scale transparent]
  GlyphGraphics
  (glyph-graphics [this]
    #_{:post [(glyph-graphics? %)]}
    (let [img (-> path-or-url
                zimg/load-image
                (zimg/mode :rgba))]
      (log/info "img" img)
      (let [characters    (map char cp437-unicode)
            font-image    (-> img
                            (zimg/copy-channels alpha)
                            (cond->
                            (> scale 1)
                              (zimg/scale scale)))
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
         :character->transparent (zipmap characters (repeat transparent))}))))


;; TODO: find a better way to keep font data around for the lifetime of STBTTFontinfo
(def font-data-buffer(atom nil))
(defrecord TTFFont [name-or-path size transparent]
  GlyphGraphics
  (glyph-graphics [this]
    (reset! font-data-buffer (font-data name-or-path))
    ;; Adjust canvas to fit character atlas size
    (let [ms (MemoryStack/create)
          ^STBTTFontinfo font-info   (make-font @font-data-buffer)
          ;_ (System/gc)
          scale                      (STBTruetype/stbtt_ScaleForPixelHeight
                                       font-info
                                       size)
          [advance
           left-side-bearing]        (map (partial * scale)
                                       (hmetrics ms
                                                 font-info
                                                 (STBTruetype/stbtt_FindGlyphIndex
                                                   font-info
                                                   (int \M))))
          codepoint->glyph-index     (displayable-characters font-info)
          char-width                 (int (Math/ceil advance))
          char-height                size
          [ascent
           descent
           line-gap]                 (vmetrics ms font-info)
          _ (log/info "characters per line" (characters-per-line char-width (count codepoint->glyph-index)))
          cwidth     2048
          cheight    (zutil/next-pow-2 (* char-height (int (Math/ceil (/ (count codepoint->glyph-index)
                                                                   (characters-per-line char-width (count codepoint->glyph-index)))))))
          width      cwidth ;(max cwidth cheight)
          height     cheight ;(max cwidth cheight)
          antialias  true
          char-idxs   (character-idxs char-width codepoint->glyph-index)]
      #_(log/info "characters" (count codepoint->glyph-index) "cwidth" cwidth "cheight" cheight)
      #_(log/info "glyph image width" width "height" height)
      #_(log/info "char-idxs" char-idxs)
      ;(log/info "characters" (vec characters))
      (log/info "character-idxs" (vec char-idxs))
      (let [texture-image (zimg/image width height)]
        ;; Loop through each character, drawing it
        (doseq [[codepoint glyph-index col row] char-idxs]
          (try
            (let [x  (* col char-width)
                  y  (* row char-height)
                  chr-img (char-image ms font-info char-width char-height ascent scale codepoint glyph-index)]
              #_(log/info "drawing" codepoint "@" x y)
              (zimg/draw-image texture-image
                             chr-img
                             x
                             y))
              (catch Throwable t
                (log/error t))))
        ;; cleanup texture resource
        (zimg/write-png texture-image "glyph-texture.png")
        {:font-texture-width width
         :font-texture-height height
         :font-texture-image texture-image
         :character-width char-width
         :character-height char-height
         :character->col-row (character->col-row char-idxs)
         :character->transparent (zipmap (map char (keys codepoint->glyph-index)) (repeat transparent))}))))

(defrecord CompositeFont [fonts]
  GlyphGraphics
  (glyph-graphics [this]
    #_{:post [(glyph-graphics? %)]}
    (let [glyphs (mapv glyph-graphics fonts)]
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
               :character->transparent character->transparent})))))))

(defrecord TileSet [path-or-url alpha tile-width tile-height margin tile-id->col-row tile-id->transparent]
  GlyphGraphics
  (glyph-graphics [this]
    #_{:post [(glyph-graphics? %)]}
    (let [font-image (-> (zimg/load-image path-or-url)
                       (zimg/mode :rgba)
                       (compact tile-width
                                tile-height
                                margin)
                       (as-> font-image
                           (if-not (= alpha :alpha)
                             (zimg/copy-channels font-image alpha)
                             font-image)))
          width         (zutil/next-pow-2 (zimg/width font-image))
          height        (zutil/next-pow-2 (zimg/height font-image))
          texture-image (zimg/resize font-image width height)]
      (zimg/write-png font-image "tileset-texture.png")
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image
       :character-width tile-width
       :character-height tile-height
       :character->col-row tile-id->col-row
       :character->transparent tile-id->transparent})))

(defn construct [font] (glyph-graphics font))
