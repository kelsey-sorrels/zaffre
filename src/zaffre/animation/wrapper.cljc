;; Functions for animating state to screen
(ns zaffre.animation.wrapper
  (:require 
            [clojure.core.async :refer :all :as async :exclude [map into]]
            [zaffre.color :as zcolor]
            ;[robinson.viewport :as rv]
            [zaffre.terminal :as zat]
            [zaffre.animation.animatedterminal :as zaat]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log]
            [overtone.at-at :as atat])
  #?(:clj
     (:import  ;zaffre.terminal.Terminal
               zaffre.animation.animatedterminal.AAnimatedTerminal
               zaffre.animation.animatedterminal.AId
               zaffre.animation.animatedterminal.AEffect
               zaffre.animation.animatedterminal.AFilter
               zaffre.animation.animatedterminal.AMask
               zaffre.animation.animatedterminal.ARequiresState
               zaffre.animation.animatedterminal.APalette)))


#?(:clj
(set! *warn-on-reflection* true))

(def frame-count (atom 0))
(def ^:dynamic *rain-rate* 0.96)


(defn size [terminal]
  ((juxt :columns :rows) (-> (zat/groups terminal) :app)))

;; util/helper fns
(defn nil-grid [terminal]
  (let [[vw vh] (size terminal)]
    (repeat vh (vec (repeat vw nil)))))

(defn lantern-flicker [opts x y fg]
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

(defn night-tint [opts x y fg]
  (if-let [day? (get opts :night-tint)]
    (zcolor/night-tint fg day?)
    fg))

(defn no-op-filter [_ _ _ bg] bg)


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

(defn render-rand-fg
  [terminal layer-id x y cell-opts cell-palette palette]
  (let [color-ids (get @palette cell-palette)
        i         (int (/ @frame-count 10))
        n         (mod (Math/abs (hash (+ x (* 13 y) (* 17 i)))) (count color-ids))
        color-id  (nth color-ids n)
        fg-rgb    (->> (zcolor/color->rgb color-id)
                    (lantern-flicker cell-opts x y)
                    (vignette cell-opts x y)
                    (night-tint cell-opts x y))]
    (zat/set-fg! terminal layer-id x y fg-rgb)))

(defrecord RandFgEffect
  [terminal mask opts palette]
  AId
  (id [this]
    :rand-fg)
  AEffect
  (apply-effect! [this terminal]
    (let [[vw vh]    (size terminal)]
      (doseq [[y line mask-line] (map vector (range) @opts @mask)
              [x cell-opts mask?] (map vector (range) line mask-line)
              :let               [cell-palette  (get cell-opts :palette)]
              :when              cell-palette]
        (render-rand-fg terminal :map x y cell-opts cell-palette palette))))
  APalette
  (update-palette! [this f]
    (swap! palette f)
    this))

(defn make-rand-fg-effect
  [terminal opts]
  (let [[vw vh]   (size terminal)
        mask      (atom (repeat vh (repeat vw true)))
        palette   (atom {})]
    (RandFgEffect. terminal mask opts palette)))

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

(defn swap-rain-mask! [terminal f]
  (zaat/swap-matching-effect-or-filter! terminal
                              (fn [fx] (= (zaat/id fx) :rain))
                              (fn [rain-fx] (zaat/swap-mask! rain-fx f))))

(defn reset-rain-mask! [terminal v]
  (zaat/swap-matching-effect-or-filter! terminal
                              (fn [fx] (= (zaat/id fx) :rain))
                              (fn [rain-fx]
                                (let [[columns rows] (size terminal)]
                                  (zaat/reset-mask! rain-fx (repeat rows (repeat columns v)))))))

(defn transform [terminal mask state transforms]
  #_(println (format "drawing %d transforms" (count transforms)))
  (doseq [transform transforms]
    (let [path (zutil/line-segment-fast-without-endpoints (get transform :from-xy) (get transform :to-xy))
          n    (int (/ @frame-count 1))]
      (when (< n (count path))
        #_(log/info "n" n "count path" (count path))
        (let [[vx vy] path #_(rv/world-xy->screen-xy state (nth path n))]
          (when (or true (get-in mask [vy vx]))
            #_(println "drawing * @ " vx vy)
            ;(zat/set-char! terminal :fx vx vy \*)
            (zat/set-fg! terminal :fx vx vy (zcolor/color->rgb :white))
            (zat/set-bg! terminal :fx vx vy (zcolor/color->rgb :black))))))))
  
(defrecord TransformEffect
  [terminal mask state]
  AId
  (id [this]
    :transform)
  AEffect
  (apply-effect! [this terminal]
    (dosync 
      (when-let [state @state]
        (transform terminal @mask state (get-in state [:fx :transform])))))
  AMask
  (swap-mask! [this f]
    (swap! mask f)
    ;(println "=====================")
    ;(doseq [line @mask]
    ;  (println (mapv (fn [v] (if v 1 0)) line)))
    this)
  (reset-mask! [this new-mask]
    (reset! mask new-mask)
    this)
  ARequiresState
  (reset-state! [this new-state]
    (reset! state new-state)
    this)
  Object
  (toString [this]
    (format "TransformEffect terminal, mask, state")))

(defn make-transform-effect
  [terminal cell-opts]
  (let [[vw vh]    (size terminal)
        mask       (atom (repeat vh (repeat vw true)))]
    (TransformEffect. terminal mask (atom nil))))

(defn blink [terminal state blinks]
  ;(prinln (format "drawing %d blinks" (count blinks)))
  (doseq [blink blinks]
    #_(println @frame-count (mod @frame-count 15))
    (when (< (mod @frame-count 15) 7)
      (let [[vx vy] (get blink :xy)]
          #_(println "drawing * @ " vx vy)
          (zat/put-chars! terminal :fx [{:x vx :y vy :c \space}])))))

(defrecord BlinkEffect
  [terminal state]
  AId
  (id [this]
    :blink)
  AEffect
  (apply-effect! [this terminal]
    (dosync 
      (when-let [state @state]
        (blink terminal state (get-in state [:fx :blink])))))
  ARequiresState
  (reset-state! [this new-state]
    (reset! state new-state)
    this)
  Object
  (toString [this]
    (format "BlinkEffect terminal, state")))

(defn make-blink-effect
  [terminal cell-opts]
  (let [[vw vh]    (size terminal)]
    (BlinkEffect. terminal (atom nil))))

(defn blip [terminal state blips]
  (doseq [blip blips]
    (let [[vx vy] blip #_(rv/world-xy->screen-xy state (get blip :xy))]
      ;(println "drawing blip" blip "@" vx vy "frame-count" @frame-count)
      ;; get the last instant before @frame-count
      (when-let [instant (->> (get blip :instants)
                              (filter (fn [instant]
                                        (< (get instant :time) @frame-count)))
                              (sort-by (fn [instant]
                                         (get instant :time)))
                              last)]
        ;(println "drawing instant" instant)
        #_(when-let [ch (get instant :ch)]
          (zat/set-char! terminal :fx vx vy ch))
        (when-let [fg (get instant :fg)]
          (zat/set-fg! terminal :fx vx vy fg))
        (when-let [bg (get instant :bg)]
          (zat/set-bg! terminal :fx vx vy bg))))))
    
(defrecord BlipEffect
  [terminal state]
  AId
  (id [this]
    :blip)
  AEffect
  (apply-effect! [this terminal]
    (dosync 
      (when-let [state @state]
        (blip terminal state (get-in state [:fx :blip])))))
  ARequiresState
  (reset-state! [this new-state]
    (reset! state new-state)
    this)
  Object
  (toString [this]
    (format "BlipEffect terminal, state")))

(defn make-blip-effect
  [terminal cell-opts]
  (let [[vw vh]    (size terminal)]
    (BlipEffect. terminal (atom nil))))

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
            effects         (atom [((make-rain-effect :fx 16 16) effects-term)])]
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

