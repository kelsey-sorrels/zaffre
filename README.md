# Zaffre

## A fast Clojure library for emulating a terminal

Zaffre is fast console library for drawing characters to a screen.

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

## Usage

Add the dependency to your project:

```clojure
[zaffre "0.4.0-SNAPSHOT"]
```

## Examples

Run with

`lein run examples.basic`

or

`lein run -m examples.tileset`

etc.


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
## License

Copyright © 2016 Aaron Santos

Distributed under the MIT license.
