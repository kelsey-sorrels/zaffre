# Zaffre

## A fast Clojure library for emulating a terminal

Zaffre is fast console library for drawing characters to a screen.

## Features
  * It's fast. Zaffre uses LWJGL and OpenGL to render characters as fast as possible.
  * Cross-platform codebase
  * Thread safe

## Not Features
  * Multiple fonts
  * Glyph stacking

## Usage

Add the dependency to your project:

```clojure
[zaffre "0.1.0-SNAPSHOT"] ; Dev
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

## License

Copyright © 2016 Kelsey Sorrels

Distributed under the MIT license.
