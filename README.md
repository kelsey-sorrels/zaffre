# Zaffre

## A fast Clojure library for emulating a terminal

Zaffre is fast console library for drawing characters to a screen.

## Features
  * It's fast. Zaffre uses LWJGL and OpenGL to render characters as fast as possible.
  * Unicode support (minus CJK code points)
  * Cross-platform codebase
  * Thread safe

## Not Features
  * Multiple fonts
  * Glyph stacking
  * Font sheets

## Usage

Add the dependency to your project:

```clojure
[zaffre "0.1.0"]
```

Setup your namespace imports:

```clojure
(ns my-clj-ns
  (:require
    [zaffre.aterminal :as zat]
    [zaffre.glterminal]
    [clojure.core.async :as async :refer [go-loop]]))
```

Create a terminal:

```clojure
  ;; render in background thread
  (let [terminal   (zaffre.glterminal/make-terminal {:title "Zaffre demo"
                                                     :columns 80 :rows 24
                                                     :default-fg-color [250 250 250]
                                                     :default-bg-color [5 5 8]
                                                     :font-size 18
                                                     :antialias true
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
       _           (go-loop []
                     (dosync
                       (doseq [x (range (count "Rainbow"))]
                         (zat/set-fx-fg! terminal (inc x) 1 [128 (rand 255) (rand 255)])))
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
                         (put-string terminal 0 0 "Hello world")
                         (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                           (put-string terminal 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                         (put-string terminal 12 0 (str key-in))
                         (put-string terminal 1 1 "Rainbow")
                         (zat/refresh! terminal)))
                         ;; ~30fps
                       (Thread/sleep 33)
                       (recur))]
```

In the main loop, read a key, process messages, and possibly change font-size:

```clojure
   ;; get key presses in fg thread
   (loop []
     (let [new-key (first (async/alts!!
                            [(async/timeout 1)
                             (zat/get-key-chan terminal)]))]
       (if new-key
         (do
           (reset! last-key new-key)
           (log/info "got key" (or (str @last-key) "nil")))
         ;; Message processing must be called in the same thread that creates the terminal.
         ;; Otherwise Windows will break.
         (zat/process-messages terminal))
       ;; change font size on s/m/l keypress
       (case @last-key
         \s (zat/apply-font! terminal "Consolas" "Monospaced" 12 true)
         \m (zat/apply-font! terminal "Consolas" "Monospaced" 18 true)
         \l (zat/apply-font! terminal "Consolas" "Monospaced" 24 true)
         nil)
       (recur))))
```

## `make-terminal` options

```clojure
(def example-options
  :title            ; String to use in window's title bar
  :columns          ; Terminal width in characters
  :rows             ; Terminal height in characters
  :default-fg-color ; [red green blue] color to use as default foreground color. (0-255)
  :default-bg-color ; [red green blue] color to use as default background color. (0-255)
  :windows-font     ; Font family, name, or path to use on Windows platform. eg: "Courier New", "./fonts/my-font.ttf"
  :else-font        ; Font family, name, or path to use on Linux and OS X platforms. eg: "Monospaced", "./fonts/my-font.ttf"
  :font-size        ; The point size of the font
  :antialias        ; Render the font with antialiasing?
  :icon-paths       ; Vector of paths to icons. eg: ["icons/16x16.png", "icons/32x32.png", "icons/128x128.png"]
                    ; Three paths for icons of three sizes must be given in this order: 16x16, 32x32, 128x128
                    )
```

All options are optional. If none are specified an empty map {} may passed to `make-terminal`.


## Terminal interface

```clojure
(defprotocol ATerminal
  ; Return [rows columns] terminal size in characters
  (get-size [this])
  ; Add characters to the back buffer
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
  (put-chars! [this characters])
  ; Change the foreground color at (x, y) where fg is [red green blue] (0-255)
  (set-fg! [this x y fg])
  ; Change the background color at (x, y) where bg is [red green blue] (0-255)
  (set-bg! [this x y bg])
  ; Returns a core.async chan that holds keyboard key presses
  (get-key-chan [this])
  ; Changes the terminal's font. Keys have the same meaning as those used in make-terminal options
  (apply-font! [this windows-font else-font size antialias])
  ; Set the terminal's cursor to x y.
  ; The character under the cursor will have their fg and bg colors swapped.
  (set-cursor! [this x y])
  ; Display the characters in the back buffer
  (refresh! [this])
  ; Clears the terminal's back buffer
  (clear! [this])
  ; Override the foreground color of the character at x y in the back buffer
  (set-fx-fg! [this x y fg])
  ; Override the background color of the character at x y in the back buffer
  (set-fx-bg! [this x y bg])
  ; Override the character at x y where c is a character in the back buffer
  (set-fx-char! [this x y c])
  ; Clear all of the fx overrides in the back buffer
  (clear-fx! [this])
  ; Must be called periodically in the same thread that created the terminal using make-terminal
  ; in order to process the windowing system messages
  (process-messages [this]))

```

## License

Copyright © 2016 Aaron Santos

Distributed under the MIT license.
