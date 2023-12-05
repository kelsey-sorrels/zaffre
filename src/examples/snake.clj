(ns examples.snake
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.animation.wrapper :as zaw]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.tilesets :as ztiles]
            [zaffre.util :as zutil]
            [clojure.core.async :refer :all :exclude [map]]))

(def font ztiles/pastiche-16x16)

(def green [  0 161  30])
(def red [255 0 0])
(def black [0 0 0])

(def width 20)
(def height 20)
(defn -main [& _]
  (zaw/create-animated-terminal
    zgl/create-terminal
    [{:id :app
      :layers [:app]
      :columns width
      :rows height
      :pos [0 0]
      :font (constantly font)}]
    {:title "Zaffre demo"     ;; Set the window title
     :screen-width (* width 16)  ;; Screen dimentions in pixels
     :screen-height (* height 16)}
    (fn [terminal] ;; Receive the terminal in a callback
      ;; Save the last key press in an atom
      (let [state    (atom :playing)
            last-key (atom :right)
            snake    (atom [[(int (/ width 2)) (int (/ height 2))]
                            [(dec (int (/ width 2))) (int (/ height 2))]])
            apple    (atom [(int (* width (rand))) (int (* height (rand)))])
            score    (atom 1)]
        ;; Every 33ms, draw a full frame
        (zat/do-frame terminal 33 
          (case @state
            :playing (do
              ;; Draw strings
              (zat/put-chars! terminal :app (zutil/mk-string 0 0 (str @score)))
              (zat/put-chars! terminal :app (map (fn [[x y]]
                                                   {:x x :y y :c \* :fg green :bg black})
                                                 @snake))
              (let [[x y] @apple]
                (zat/put-chars! terminal :app [{:x x :y y :c \* :fg red :bg black}])))
           :game-over (do
              (zat/put-chars! terminal :app (zutil/mk-string (- (quot width 2) 4) (dec (quot height 2))
                                              (str "Game over.")))
              (zat/put-chars! terminal :app (zutil/mk-string 0 (inc (quot height 2))
                (format "You scored %d points." @score))))))
        ;; Wire up terminal events to channels we read from
        (zevents/add-event-listener terminal :keypress
           (fn [new-key]
            ;; Make the `q` key quit the application
            (case new-key
              \w (reset! last-key :up)
              \a (reset! last-key :left)
              \s (reset! last-key :down)
              \d (reset! last-key :right)
              \q (zat/destroy! terminal)
              \  (when (= @state :game-over)
                   (reset! snake  [[(int (/ width 2)) (int (/ height 2))]
                          [(dec (int (/ width 2))) (int (/ height 2))]])
                   (reset! apple [(int (* width (rand))) (int (* height (rand)))])
                   (reset! score 1)
                   (reset! last-key :right)
                   (reset! state :playing))
              nil)))
        ;; game loop
        (go-loop []
          ;; Update 
          (when (= @state :playing)
            (let [[x y] (first @snake)
                  next-head (case @last-key
                              :up    [x (dec y)]
                              :down  [x (inc y)]
                              :left  [(dec x) y]
                              :right [(inc x) y])]
              ;; check self intersection and bounds intersection
              (if (or (some (partial = next-head) @snake)
                      (not (< -1 (first next-head) width))
                      (not (< -1 (second next-head) height)))
                ;; gameover
                (reset! state :game-over)
                ;; else update snake
                (swap! snake (fn [snake]
                    (cons next-head
                          ;; If the snake head hits the apple, don't subtract from the tail
                          (if (= [x y] @apple)
                            (do
                              (swap! score inc)
                              (reset! apple [(int (* width (rand))) (int (* height (rand)))])
                              snake)
                            (butlast snake))))))))
          (<! (timeout 400))
          (recur))))))

