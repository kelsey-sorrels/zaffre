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


