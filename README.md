# Zaffre

## A fast Clojure library for emulating a terminal

Zaffre is fast console library for drawing characters to a screen.

## Features
  * It's fast. Zaffre uses LWJGL and OpenGL to render characters as fast as possible.
  * Unicode support (minus CJK code points)
  * CP437 tileset support (eg: loading [Dwarf Fortress Wiki: Tileset repository](http://dwarffortresswiki.org/index.php/Tileset_repository))
  * Cross-platform codebase
  * Thread safe

## Not Features
  * Multiple fonts
  * Glyph stacking
  * Non-character tiles

## Usage

Add the dependency to your project:

```clojure
[zaffre "0.4.0"]
```

Setup your namespace imports:

```clojure
(ns my-clj-ns
  (:require
    [zaffre.aterminal :as zat]
    [zaffre.glterminal]
    [clojure.core.async :as async :refer [go-loop]])
  (:import
    (zaffre.font CP437Font TTFFont)))
```

Create a terminal:

```clojure
  ;; render in background thread
  (let [terminal   (zaffre.glterminal/make-terminal
                     ;; Two layers, :text on the bottom and :rainbow on top
                     [:text :rainbow]
                     {:title "Zaffre demo"
                      :columns 80 :rows 24
                      :default-fg-color [250 250 250]
                      :default-bg-color [5 5 8]
                      ;; Specify font to use for windows platform
                      ;; In this case let's pull a font from the dwarf fortress tileset repository
                      ;; Use the :green channel and a scale of 1. (2 would, for example, be twice as large)
                      :windows-font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1)
                      ;; Specify font to use for non-windows platforms
                      ;; Here we'll load a ttf font by font-family. A font name or even a filesystem path can be used too
                      :else-font (TTFFont. "Monospaced" 16)
                      ;; Use fullscreen for fun
                      :fullscreen true
                      :icon-paths ["images/icon-16x16.png"
                                   "images/icon-32x32.png"
                                   "images/icon-128x128.png"]})
```

Store some state, in this case the last key that was pressed:

```clojure
       last-key    (atom nil)
```

Start a background job that changes text colors:

```clojure
       ;; Every 10ms, set the "Rainbow" text to have a random fg color
       fx-chan     (go-loop []
                     (dosync
                       (doseq [x (range (count "Rainbow"))]
                         (zat/set-fx-fg! terminal :rainbow (inc x) 1 [128 (rand 255) (rand 255)])))
                       (zat/refresh! terminal)
                     (Thread/sleep 10)
                     (recur))
```

Start a background job to render at 30fps:

```clojure
       ;; Every 33ms, draw a full frame
       render-chan (go-loop []
                     (dosync
                       (let [key-in (or @last-key \?)]
                         (zat/clear! terminal)
                         (put-string terminal :text 0 0 "Hello world")
                         (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                           (put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                         (put-string terminal :text 12 0 (str key-in))
                         (put-string terminal :rainbow 1 1 "Rainbow")
                         (zat/refresh! terminal)))
                         ;; ~30fps
                       (Thread/sleep 33)
                       (recur))]
```

In the main loop, read a key, process messages, and possibly change font-size:

```clojure
   ;; get key presses in fg thread
   (loop []
     (let [new-key (async/<!! (zat/get-key-chan terminal))]
       (if (= new-key :exit)
         (do
           (async/close! fx-chan)
           (async/close! render-chan)
           (System/exit 0))
         (do
           (reset! last-key new-key)
           (log/info "got key" (or (str @last-key) "nil")))
           ;; change font size on s/m/l keypress
           (case @last-key
             \s (zat/apply-font! terminal (TTFFont. "Consolas" 12) (TTFFont. "Monospaced" 12))
             \m (zat/apply-font! terminal (TTFFont. "Consolas" 18) (TTFFont. "Monospaced" 18))
             \l (zat/apply-font! terminal (TTFFont. "Consolas" 24) (TTFFont. "Monospaced" 24))
             nil)
           (recur))))
```

## `make-terminal` parameters

The terminal creation function `make-terminal` takes two parameters: `layer-order` and `opts`.

`layer-order` a vector of layer identifiers. The order specifies the ordering of layers from bottommost to topmost.
Most often this will be a series of keywords like [:base :text :fx :ui].

All options are optional. If none are specified an empty map {} may passed to `make-terminal`.
```clojure
(def example-options
  :title            ; String to use in window's title bar
  :columns          ; Terminal width in characters
  :rows             ; Terminal height in characters
  :default-fg-color ; [red green blue] color to use as default foreground color. (0-255)
  :default-bg-color ; [red green blue] color to use as default background color. (0-255)
  :windows-font     ; TTFFont or CP437Font to use on Windows platform. 
  :else-font        ; TTFFont or CP437Font to use on Linux and OS X platforms. 
  :icon-paths       ; Vector of paths to icons. eg: ["icons/16x16.png", "icons/32x32.png", "icons/128x128.png"]
                    ; Three paths for icons of three sizes must be given in this order: 16x16, 32x32, 128x128
  :fx-shader        ; Omit or specify a map wih
                    ; {:name ;"retro.fs" or path to GLSL fragment shader
                    ;  :uniforms ; a vector of vectors. Each element contains a ["name", value] pair like so
                    ;             [["time" 0]
                    ;              ["noise" 0.0016]
                    ;              ["colorShift" 0.0001]
                    ;              ["scanlineDepth" 0.94]
                    ;              ["brightness" 0.68]
                    ;              ["contrast" 2.46]]
                    )
```



## Terminal interface

```clojure
(defprotocol ATerminal
  ; Return [rows columns] terminal size in characters
  (get-size [this])
  ; Add characters to the back buffer
  ; Layer id  must match one of the layers specified in the `make-terminal` `layer-order` parameter
  ; Characters is a vector of maps each with these keys
  ; {:c \a             ; The character to place
  ;  :fg [255 255 255] ; [red green blue] foreground color. (0-255)
  ;  :bg [0 0 0]       ; [red green blue] background color. (0-255)
  ;  :x  0             ; column to place the character
  ;  :y  0             ; row to place the character
  ; The terminal coordinate system is layed out like this
  ; (0, 0)          (cols, 0)
  ;  +---------------+
  ;  |               |
  ;  |               |
  ;  |               |
  ;  +---------------+
  ; (0, rows)          (cols, rows)
  (put-chars! [this layer-id characters])
  ; Change the foreground color at (x, y) where fg is [red green blue] (0-255)
  (set-fg! [this layer-id x y fg])
  ; Change the background color at (x, y) where bg is [red green blue] (0-255)
  (set-bg! [this layer-id x y bg])
  ; Update the fx shader's uniform values
  ; The key must match one of the uniforms specified in `make-terminal`'s options
  ; The value must be convertable to a double
  (assoc-fx-uniform! [this k v])
  ; Returns a core.async chan that holds keyboard key presses
  (get-key-chan [this])
  ; Changes the terminal's font.
  ; Any combination of TTFFont or CP437Font records may be used.
  (apply-font! [this windows-font else-font])
  ; Set the terminal's cursor to x y.
  ; The character under the cursor will have their fg and bg colors swapped.
  (set-cursor! [this x y])
  ; Display the characters in the back buffer
  (refresh! [this])
  ; Clears the terminal's back buffer
  (clear! [this]
  ; Clear a specific layer
          [this layer-id])
  ; Switch to or from fullscreen mode
  ; Passing `false` disables fullscreen
  ; Using one of the values returned from `fullscreen-sizes` will force the terminal to switch
  ; to fullscreen mode at that resolution
  (fullscreen! [this v])
  ;; Returns a seq of valid fullscreen resolutions each of which is a [width height obj]
  ; `width` and `height` are integer values and `obj` is an opaque object used internally
  (fullscreen-sizes [this])
  ; Override the foreground color of the character at x y in the back buffer
  (set-fx-fg! [this layer-id x y fg])
  ; Override the background color of the character at x y in the back buffer
  (set-fx-bg! [this layer-id x y bg])
  ; Override the character at x y where c is a character in the back buffer
  (set-fx-char! [this layer-id x y c])
  ; Clear all of the fx overrides in the back buffer
  (clear-fx! [this]
  ; Clear the fx of a specific layer
             [this layer-id])
  ; Destroy terminal and end processing.
  (destroy! [this]))

```

## License

Copyright © 2016 Aaron Santos

Distributed under the MIT license.
