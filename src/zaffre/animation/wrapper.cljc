;; Functions for animating state to screen
(ns zaffre.animation.wrapper
  (:require 
            [clojure.core.async :refer :all :as async :exclude [map into]]
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


;;; Effects

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
       :fg (zcolor/color->rgb :blue)
       :bg (zcolor/color->rgb :black)}
    :splash1
      {:c  \*
       :x x :y y
       :fg (zcolor/color->rgb :blue)
       :bg (zcolor/color->rgb :black)}
    :splash2
      {:c  \o
       :x x :y y
       :fg (zcolor/color->rgb :blue)
       :bg (zcolor/color->rgb :black)}
    :splash3
      {:c  \°
       :x x :y y
       :fg  (zcolor/color->rgb :blue)
       :bg  (zcolor/color->rgb :black)}
    :splash4
      {:c  \'
       :x x :y y
       :fg  (zcolor/color->rgb :blue)
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

(defrecord RainEffect
  [terminal layer-id rain-state]
  ACmdStream
  (stream [this]
    (let [dt      33
          [vw vh] (size terminal)]
      (go-loop [rain-state rain-state]
        (<! (timeout dt))
        (zat/clear! terminal layer-id)
        (let [chrs (for [[y line] (map-indexed vector rain-state)
                         [x cell] (map-indexed vector line)
                         :when cell]
                     (rain-cell-char x y cell))]
          (zat/put-chars!
             terminal
             layer-id
             chrs)
          (zat/refresh! terminal)
          (recur (step-rain rain-state vw vh)))))))

(defn make-rain-effect
  [layer-id vw vh]
  (let [rain-state (repeat vh (vec (repeat vw nil)))]
    (fn [terminal]
      (RainEffect. terminal layer-id rain-state))))

(defrecord TransformEffect
  [terminal layer-id ch from to duration]
  ACmdStream
  (stream [this]
    (let [path (zutil/line-segment-fast-without-endpoints from to)
          dt   (* 1000 (/ duration (count path)))]
      (go-loop [[x y] (first path) xys (next path)]
        (zat/clear! terminal layer-id)
        (zat/put-chars! terminal layer-id [(assoc ch :x x :y y)])
        (zat/refresh! terminal)
        (<! (timeout dt))
        (if xys
          (recur (first xys) (next xys)))))))

(defn make-transform-effect
  [terminal layer-id ch from to duration]
    (fn [terminal]
      (TransformEffect. terminal layer-id ch from to duration)))

(defrecord BlinkEffect
  [terminal layer-id characters initial intervals]
  ACmdStream
  (stream [this]
    (let [dt 1000]
      (go-loop [state initial]
        (zat/clear! terminal layer-id)
        (when state
          (zat/put-chars! terminal layer-id characters))
        (zat/refresh! terminal)
        (<! (timeout dt))
        (recur (not state))))))

(defn make-blink-effect
  ([layer-id characters]
    (make-blink-effect layer-id characters true))
  ([layer-id characters initial]
    (make-blink-effect layer-id characters initial (constantly 1000)))
  ([layer-id characters initial intervals]
    (fn [terminal]
      (BlinkEffect. terminal layer-id characters initial intervals))))

(defn make-blip-effect
  [layer-id characters duration]
  (make-blink-effect layer-id characters [duration]))

;;; Queue cmds in a chan and foreard them to dst-chan on `(refresh)`.
(defrecord WrappedAnimatedTerminal [terminal cmds dst-chan]
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
    (swap! cmds conj (list zat/refresh! terminal))
    (>!! dst-chan @cmds)
    (reset! cmds []))
  (clear! [_]
    (swap! cmds conj (list zat/clear! terminal)))
  (clear! [_ layer-id] (swap! cmds conj (list zat/clear! terminal layer-id)))
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
      (let [effects-gen-fns (get opts :effects)
            started         (atom false)
            ;; grid of cell-opts
            cmd-chan        (chan 1000)
            effects-cmd-chan (chan 1000)
            wrapped-term    (WrappedAnimatedTerminal. terminal (atom []) cmd-chan)
            effects-term    (WrappedAnimatedTerminal. terminal (atom []) cmd-chan)
            ;effects         (atom [((make-rain-effect :fx 16 16) effects-term)])]
            effects         (atom [((make-blink-effect :fx [{:x 8 :y 8 :c \* :fg [128 128 0] :bg [0 0 0]}] true) effects-term)])]
        #_(log/info "overriding terminal")
        #_(log/info "effects" @effects "filters" @filters)
        ;; start effects
        (doseq [effect @effects]
          (stream effect))
        ;; start a background loop that renders events
        (go-loop [cmds (<! cmd-chan)]
          #_(log/info "rendering" (count cmds) "cmds")
          (dosync
            (doseq [cmd cmds]
              #_(log/info "invoking" (first cmd) (rest (rest cmd)))
              (apply (first cmd) (rest cmd))))
          (recur (<! cmd-chan)))
         (f wrapped-term)))))

