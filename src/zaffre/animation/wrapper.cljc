;; Functions for animating state to screen
(ns zaffre.animation.wrapper
  (:require 
            [clojure.core.async :refer :all :as async :exclude [into map reduce take]]
            [zaffre.color :as zcolor]
            [zaffre.terminal :as zat]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log]
            [overtone.at-at :as atat]))

#?(:clj
(set! *warn-on-reflection* true))

(def ^:dynamic *rain-rate* 0.96)


(defn size [terminal]
  ((juxt :columns :rows) (-> (zat/groups terminal) :app)))

;; util/helper fns
(defn nil-grid [terminal]
  (let [[vw vh] (size terminal)]
    (repeat vh (vec (repeat vw nil)))))

#_(defn lantern-flicker [opts x y fg]
  {:post [(vector? %)]}
  (if (get opts :lantern-flicker)
    (let [v                   (int (* 50 (/ (Math/abs (hash @frame-count)) Integer/MAX_VALUE)))
          lantern-flicker-rgb [v v 0]]
      (zcolor/add-rgb fg lantern-flicker-rgb))
    fg))

(defn vignette [opts x y fg]
  (if-let [distance (get opts :distance-from-player)]
    (do
      (zcolor/darken-rgb fg
                         (min 1 (/ 2 (max 1 distance)))))
    fg))


;;; Effect helpers

(defn rain-transition
  [advanced-cell old-cell]
  (cond
    ;; drop->nil
    ;; drop->drop
    (= advanced-cell :drop)
      (cond
        (> (rand) 0.95)
          nil
        (> (rand) 0.95)
          :splash1
        :else
          :drop)
    ;; drop->splash
    (= old-cell :splash1)
      :splash2
    (= old-cell :splash2)
      :splash3
    (= old-cell :splash3)
      :splash4
    (= old-cell :splash4)
      nil
    :else
    nil))

(defn rain-cell-char
  [x y cell] 
  (case cell
    :drop
      {:c \|
       :x x :y y
       :fg (zcolor/color->rgb :white)
       :bg (zcolor/color->rgb :black)}
    :splash1
      {:c  \*
       :x x :y y
       :fg (zcolor/color->rgb :white)
       :bg (zcolor/color->rgb :black)}
    :splash2
      {:c  \o
       :x x :y y
       :fg (zcolor/color->rgb :white)
       :bg (zcolor/color->rgb :black)}
    :splash3
      {:c  \°
       :x x :y y
       :fg  (zcolor/color->rgb :white)
       :bg  (zcolor/color->rgb :black)}
    :splash4
      {:c  \'
       :x x :y y
       :fg  (zcolor/color->rgb :white)
       :bg  (zcolor/color->rgb :black)}
    nil))

(defn render-rain-cell
  [terminal layer-id x y cell] 
  (case cell
    :drop
      (do
        (zat/put-chars! terminal layer-id [
          {:c \|
           :x x :y y
           :fg (zcolor/color->rgb :blue)
           :bg (zcolor/color->rgb :black)}]))
    :splash1
      (do
        (zat/put-chars! terminal layer-id [
          {:c  \*
           :x x :y y
           :fg (zcolor/color->rgb :blue)
           :bg (zcolor/color->rgb :black)}]))
    :splash2
      (do
        (zat/put-chars! terminal layer-id [
          {:c  \o
           :x x :y y
           :fg (zcolor/color->rgb :blue)
           :bg (zcolor/color->rgb :black)}]))
    :splash3
      (do
        (zat/put-chars! terminal layer-id [
          {:c  \°
           :x x :y y
           :fg  (zcolor/color->rgb :blue)
           :bg  (zcolor/color->rgb :black)}]))
    :splash4
      (do
        (zat/put-chars! terminal layer-id [
          {:c  \
           :x x :y y
           :fg  (zcolor/color->rgb :blue)
           :bg  (zcolor/color->rgb :black)}]))
    nil))

(defn step-rain
  [rain-state vw vh]
  ;; update rain
  ;(println "------------------------------------------------------")
  (vec  (for [[y advanced-line old-line] (map vector (range)
                                                     (concat [(repeat vw nil)] (butlast rain-state))
                                                     rain-state)]
     (do
     ;(println (mapv (fn [v] (if v 1 0)) old-line))
     ;; prev-cell is the value above `cell`.
     (vec (for [[x advanced-cell old-cell] (map vector (range) advanced-line old-line)]
       (if (zero? y)
         (if (> (rand) *rain-rate*)
           :drop
           nil)
         (rain-transition advanced-cell old-cell))))))))


(defprotocol ACmdStream
  (stream [this]))

;;; Event Streams
;; s is a seq of [dt [layer-id [{:c \* :x 1 :y 2 :fg [r g b] :bg [r g b]}...]]]
(defrecord SeqEffect
  [terminal s]
  ACmdStream
  (stream [this]
    (go-loop [x (first s) xs (next s)]
      (let [[_ {:keys [layer-id]}] x]
        #_(zat/clear! terminal layer-id))
      (let [[dt {:keys [layer-id characters]}] x]
        (zat/put-chars! terminal layer-id characters)
        (zat/refresh! terminal)
        (<! (timeout dt))
        (when xs
          (recur (first xs) (next xs)))))))

(defn effect-from-events
  [s]
  (fn [terminal]
    (->SeqEffect terminal s)))

(defn delay-events [duration s]
  (cons [duration []] s))
  
(defn merge-events-helper [ss]
  (cond
    (empty? ss) []
    ;; sort 
    (seq ss)
    (let [[[now & xs] & upcoming] (sort-by ffirst (remove empty? ss))]
      (if now
        (let [
            ;_ (println "now" now)
            duration         (first now)
            ;_ (println "duration" duration)
            ;_ (println "xs" xs)
            ;_ (println "upcoming" upcoming)
            new-upcoming     (map (fn [s]
                                    (cons (update (first s) 0 #(- % duration)) (rest s)))
                                  (or upcoming []))
            ;_ (println "new-upcoming" new-upcoming)
            new-ss           (if xs (cons xs new-upcoming) new-upcoming)]
        ;(println "new-ss" new-ss)
        (cons now (lazy-seq (merge-events-helper new-ss))))
      []))))

(defn- filter-events [s]
  (remove (fn [e] (and (vector? e)
                       (= 2 (count e))
                       (zero? (first e))
                       (or (seq? (second e))
                           (vector? (second e)))
                       (empty? (second e))))
          s))

(defn merge-events
  ([s] (filter-events s))
  ([s & ss] (filter-events (merge-events-helper (cons s ss)))))

;;; Effects
(defn make-rain-effect
  [layer-id vw vh]
  (let [rain-state (repeat vh (vec (repeat vw nil)))]
    (map (fn [rain-state]
           [33 {:layer-id layer-id
                :characters (for [[y line] (map-indexed vector rain-state)
                                  [x cell] (map-indexed vector line)
                                  :when cell]
                              (rain-cell-char x y cell))}])
         (iterate #(step-rain % vw vh) rain-state))))

(defn make-transform-effect
  [layer-id ch from to duration]
    (let [path (zutil/line-segment-fast-without-endpoints from to)
          dt   (* 1000 (/ duration (count path)))]
      (map (fn [[x y]]
             [dt {:layer-id layer-id
                  :characters [(assoc ch :x x :y y)]}])
           path)
    #_(TransformEffect. terminal layer-id ch from to duration)))

(defn make-blink-effect
  ([layer-id characters]
    (make-blink-effect layer-id characters true))
  ([layer-id characters initial]
    (make-blink-effect layer-id characters initial (repeat 1000)))
  ([layer-id characters initial intervals]
    (drop (if initial 1 0)
          (map vector intervals (interleave (repeat {:layer-id layer-id :characters []})
                                            (repeat {:layer-id layer-id :characters characters}))))))

(defn make-blip-effect
  [layer-id characters duration]
  (take 2 (make-blink-effect layer-id characters true (repeat duration))))

(defn make-lighting-effect
  "Take a layer-id width hight and lights and returns an event stream
   where each light looks like {:x x :y y :z z :color [r g b]}.
   Additionally each light may include an :intensity float param. If
   not supplies a default value of 1.0 will be used."
  [layer-id width height lights]
  (repeat
    [0 {:layer-id layer-id
        :characters
          (for [x (range width)
                y (range height)]
            {:x x
             :y y
             :c \█
             :bg [0 0 0]
             :fg (mapv int
                   (reduce
                     (fn [color1 color2]
                       (map + color1 color2))
                     [0 0 0]
                     (map (fn [{lx :x ly :y lz :z intensity :intensity color :color}]
                            (let [dx (- x lx)
                                  dy (- y ly)
                                  dz lz
                                  i  (* (or intensity 1) (zutil/light-intensity dx dy dz))]
                              (map (partial * i) color)))
                           lights)))})}]))
                  
;;; Queue cmds in a chan and foreard them to last-cmds on `(refresh)`.
(defrecord BufferedTerminal [terminal cmds last-cmds]
  zat/Terminal
  ;; pass Terminal methods through to terminal
  (args [_] (zat/args terminal))
  (groups [_] (zat/groups terminal))
  (alter-group-pos! [_ group-id pos-fn] (swap! cmds conj (list zat/alter-group-pos! terminal group-id pos-fn)))
  (alter-group-font! [_ group-id font-fn] (swap! cmds conj (list zat/alter-group-font! terminal group-id font-fn)))
  (put-chars! [_ layer-id characters] (swap! cmds conj (list zat/put-chars! terminal layer-id characters)))
  (set-fg! [_ layer-id x y fg] (swap! cmds conj (list zat/set-fg! terminal layer-id x y fg)))
  (set-bg! [_ layer-id x y bg] (swap! cmds conj (list zat/set-bg! terminal layer-id x y bg)))
  (assoc-shader-param! [_ k v] (swap! cmds conj (list zat/assoc-shader-param! terminal k v)))
  (pub [_] (zat/pub terminal))
  (refresh! [_]
    #_(swap! cmds conj (list zat/refresh! terminal))
    (reset! last-cmds @cmds)
    (reset! cmds []))
  (clear! [_]
    (swap! cmds conj (list zat/clear! terminal)))
  (clear! [_ layer-id]
    (swap! cmds conj (list zat/clear! terminal layer-id)))
  (set-window-size! [_ v] (swap! cmds conj (list zat/set-window-size! terminal v)))
  (fullscreen-sizes [_] (zat/fullscreen-sizes terminal))
  (destroy! [_] (zat/destroy! terminal))
  (destroyed? [_] (zat/destroyed? terminal)))

(defn create-animated-terminal
  [terminal-fn group-map opts f]
  (terminal-fn
    group-map
    opts
    (fn [terminal]
      (let [effects-gen-fns   (get opts :effects)
            dt                33
            wrapped-term-cmds (atom [])
            wrapped-term      (->BufferedTerminal terminal (atom []) wrapped-term-cmds)
            ;effects           (atom [((make-rain-effect :fx 16 16) effects-term)])]
            ; seq [effect cmds-atom]
            effects           (map (fn [effect]
                                     (let [effect-cmds (atom nil)]
                                       [(->SeqEffect (->BufferedTerminal terminal (atom []) effect-cmds) effect) effect-cmds]))
                                   (get opts :effects)
                                   #_[(make-transform-effect :fx {:c \@ :fg [255 255 255] :bg [0 0 0]} [0 8] [16 8] 5)
                                    (make-rain-effect :fx 16 16)
                                    (make-blip-effect :fx [{:x 8 :y 8 :c \* :fg [128 128 0] :bg [0 0 0]}] 1000)
                                    (make-blink-effect :fx [{:x 4 :y 4 :c \* :fg [255 128 0] :bg [0 0 0]}] false)
                                    (make-blink-effect :fx [{:x 8 :y 8 :c \* :fg [128 128 0] :bg [0 0 0]}] true)])]
        #_(log/info "overriding terminal")
        #_(log/info "effects" @effects "filters" @filters)
        ;; start effects
        (doseq [[effect _] effects]
          (stream effect))
        ;; start a background loop that renders events
        (go-loop []
          #_(log/info "rendering" (count cmds) "cmds")
          (dosync
            (zat/clear! terminal)
            (doseq [cmd @wrapped-term-cmds]
              (apply (first cmd) (rest cmd)))
            (doseq [[_ cmds] effects
                    cmd      @cmds]
              #_(log/info "invoking" (first cmd) (rest (rest cmd)))
              (apply (first cmd) (rest cmd)))
            (zat/refresh! terminal))
          (<! (timeout dt))
          (recur))
         (f wrapped-term)))))

