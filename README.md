# Zaffre

## A fast Clojure library for emulating a terminal

Zaffre is fast console library for drawing characters to a screen.

<img src="https://cloud.githubusercontent.com/assets/1139945/14769786/a4815352-0a15-11e6-8d01-38d650a0a212.png" alt="Screenshot with 2-bit tileset by Muziak" title="" align="right" />


## Features
  * It's fast. Zaffre uses LWJGL and OpenGL to render characters as fast as possible.
  * Unicode support (minus CJK code points)
  * CP437 tileset support (eg: loading [Dwarf Fortress Wiki: Tileset repository](http://dwarffortresswiki.org/index.php/Tileset_repository))
  * Cross-platform codebase
  * Thread safe
  * Multiple fonts
  * Glyph stacking
  * Non-character tiles ie: sprites
  * Mix different font sizes

## Not Features
  * Effects
  * Animation
  * GUI control emulation

<img src="https://cloud.githubusercontent.com/assets/1139945/14769784/a40b1aa2-0a15-11e6-9252-8d6598e697bb.png" alt="Screenshot using 16x16 Fantasy tileset by Jerom" title="" align="right" />

## Usage

Add the dependency to your project:

```clojure
[zaffre "0.4.0-SNAPSHOT"]
```

## Quickstart

Add zaffre as a dependency to your project. `[zaffre "0.4.0-SNAPSHOT]`

Run a little hello world terminal

```clojure
(ns examples.basic
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [<! <!! go-loop]]))

(defn -main [& _]
   (zgl/create-terminal
     {:app {           ;; Setup a layer group `:app`
       :layers [:text] ;; With one layer `:text`
       :columns 16     ;; 16 characters wide
       :rows 16        ;; 16 characters tall
       :pos [0 0]      ;; With no position offset
       :font (constantly ztiles/pastiche-16x16)}} ;; Give the group a nice font
     {:title "Zaffre demo"     ;; Set the window title
      :screen-width (* 16 16)  ;; Screen dimentions in pixels
      :screen-height (* 16 16)} ;; Since our font is 16x16 and our layer group
                                ;; is also 16x16
     (fn [terminal]     ;; Receive the terminal in a callback
       (let [term-pub   (zat/pub terminal) ;; Save the terminal pub(lication)
             key-chan   (async/chan) ;; Setup a channel to receive key presses
             close-chan (async/chan) ;; Setup a channle to recive the close message
             last-key   (atom nil)   ;; Save the last key press in an atom
             ;; Every 33ms, draw a full frame
             render-chan (go-loop []
                           (dosync
                             (let [key-in (or @last-key \?)]
                               ;; For each frame
                               (zat/clear! terminal) ;; Clear the terminal
                               ;; Draw strings
                               (zutil/put-string terminal :text 0 0 "Hello world")
                               (zutil/put-string terminal :text 12 0 (str key-in))
                               ;; Draw the characters on the screen
                               (zat/refresh! terminal)))
                               ;; 33ms = ~30fps
                             (Thread/sleep 33)
                             (recur))
             ;; Receive key presses
             input-chan (go-loop []
                          (let [new-key (<! key-chan)]
                            ;; Save last key
                            (reset! last-key new-key)
                            ;; Make the `q` key quit the application
                            (case new-key
                              \q (zat/destroy! terminal)
                              nil)
                            (recur)))]
        ;; Wire up terminal events to channels we read from
        (async/sub term-pub :keypress key-chan)
        (async/sub term-pub :close close-chan)
        ;; Block until a message is received on the close channel
        (<!! close-chan)))))
```


<img src="https://cloud.githubusercontent.com/assets/1139945/14769783/a3f2141c-0a15-11e6-9103-be02a9809c8c.png" alt="Screenshot" title="" align="right" />

## More Examples

Run with

`lein run examples.basic`

or

`lein run -m examples.tileset`

etc.


## License

Copyright © 2016 Kelsey Sorrels

Distributed under the MIT license.
