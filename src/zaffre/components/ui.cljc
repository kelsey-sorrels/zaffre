(ns zaffre.components.ui
  (:require
    [overtone.at-at :as atat]
    [clj-http.client :as client]
    [clojure.java.io :as jio]
    [clojure.core.cache :as cache]
    [clojure.core.async :refer [>! <! chan to-chan timeout merge go go-loop thread]]
    [clojure.pprint :as pprint]
    [taoensso.timbre :as log]
    rockpick.core
    [zaffre.components :as zc]
    [zaffre.font :as zfont]
    [zaffre.imageutil :as ziu]))

(log/set-level! :info)

(def resource-cache (atom (cache/ttl-cache-factory {} :ttl 30000)))
(def image-cache (atom (cache/fifo-cache-factory {} :ttl 30000)))

(declare Input)
(declare InputSelect) 

(defn input-element-seq [dom]
  (zc/filter-element-tree (fn [{:keys [type]}]
                            (= type Input))
                          dom))

(defn input-select-element [dom]
  (first
    (zc/filter-element-tree (fn [{:keys [type]}]
							  (= type InputSelect))
							dom)))

(defn to-input-char [event]
  (if (= event :space)
    \ 
    event))

(def InputSelect (zc/create-react-class {
  :display-name "InputSelect"
  :get-initial-state (fn []
    {:index 0})
  :get-default-props (fn input-get-default-props [] {
    :on-keypress (fn input-select-on-keypress [this e]
                   (log/info "InputSelect on-keypress" (get e :key))
                   (let [{:keys [key dom]} e
                         {:keys [index]} (zc/state this)
                         input-elements (input-element-seq dom)]
                     (when-not (empty? input-elements)
                       (let [curr-input (nth input-elements (mod index (count input-elements)))]
                         (if (= key :tab)
                           (let [next-input (nth input-elements (mod (inc index) (count input-elements)))]
                             (when-not (= curr-input next-input)
                               ;; blur curr input
                               (binding [zc/*current-owner* curr-input]
                                 (let [instance (zc/construct-instance curr-input)
                                     {:keys [on-blur]} (zc/props instance)]
                                   (on-blur
                                     (assoc instance :updater zc/*updater*)
                                     {})))
                               ;; focus next input
                               (binding [zc/*current-owner* next-input]
                                 (let [instance (zc/construct-instance next-input)
                                     {:keys [on-focus]} (zc/props instance)]
                                   (on-focus
                                     (assoc instance :updater zc/*updater*)
                                     {})))
                               ;; update this state
                               (zc/set-state! this (fn [{:keys [index]}]
                                                     {:index (inc index)}))))
                            ;; dispatch event to curr input
                            (binding [zc/*current-owner* curr-input]
                              (let [instance (zc/construct-instance curr-input)
                                  {:keys [on-keypress]} (zc/props instance)]
                                (on-keypress
                                  (assoc instance :updater zc/*updater*)
                                  {:key key}))))))))})
  :render
    (fn [this]
      (let [{:keys [children] :as props} (zc/props this)]
        (log/trace "InputSelect render")
        (first children)))}))
  
(def c (atom 0))
(def Input (zc/create-react-class {
    :display-name "Input"
    :get-initial-state (fn []
                         (log/info "Input get-initial-state" (zc/element-id-str zc/*current-owner*))
                         (when (< 10 @c )
                           (assert false (zc/element-id-str zc/*current-owner*)))
                         (swap! c inc)
                         (assert (not (nil? zc/*current-owner*)) "Input *current-owner* nil")
                         (assert (not= (zc/element-id-str zc/*current-owner*) "null") "Input *current-owner* null")
                         {:value ""
                          :show-cursor false
                          :focused false})
    :get-default-props (fn input-get-default-props [] {
      :max-length 28
      :style {:width 30
              :height 1
              :cursor-char-on \u2592
              :cursor-char-off \space
              :cursor-fg [255 255 255]
              :cursor-bg [0 0 0]}
      :on-focus (fn [this e]
                  (zc/set-state! this (fn [s] (merge s {:focused true}))))
      :on-blur (fn [this e]
                  (zc/set-state! this (fn [s] (merge s {:focused false}))))
      :on-keypress (fn input-on-keypress [this e]
                     (log/info "Input on-keypress" e)
                     (let [{:keys [max-length]} (zc/props this)
                           k (get e :key)]
                       (cond
                         (= k :backspace)
                             (zc/set-state! this (fn [{:keys [value]}]
                                                   {:value (subs value 0 (dec (count value)))}))
                         (and (or (char? k) (= k :space)) (not= k \newline))
                           (zc/set-state! this (fn [{:keys [value]}]
                                                 (if (< (count value) max-length)
                                                   {:value (str value (to-input-char k))}
                                                   {:value value}))))))})
    :render
      (fn [this]
        (let [{:keys [value focused]} (zc/state this)
              {:keys [style] :as props} (zc/props this)
              {:keys [cursor-char-on cursor-char-off
                      cursor-fg cursor-bg]}  style
              duty-on 400
              duty-off 400
              t (mod (System/currentTimeMillis) (+ duty-on duty-off))
              show-cursor (< t duty-on)
              cursor (if (and focused show-cursor) cursor-char-on cursor-char-off)]
          (log/debug "Input render" show-cursor (dissoc props :children))
          (zc/csx [:view {:style {:border-style :single
                                  :border-bottom 1}} [
                    [:text {} [
                      [:text {} [value]]
                      [:text {:style {:fg cursor-fg :bg cursor-bg}} [(str cursor)]]]]]])))}))

(def FileResource (zc/create-react-class {
  :display-name "FileResource"
  :get-initial-state (fn [] {:state :not-loaded})
  :component-will-mount (fn [this]
    (log/info "FileResource component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [src child-type]} (zc/props this)]
      (zc/set-state! this {:state :loading})
      (future
        (try
          (binding [zc/*current-owner* owner
                    zc/*updater* updater]
            (log/info "FileResource component-will-mount load-fn " src " " (zc/element-id-str zc/*current-owner*))
            (let [bytes (get (swap! resource-cache
                                    cache/through-cache
                                    src
                                    (constantly (ziu/slurp-bytes (jio/as-file src))))
                             src)]
              (log/info "FileResource loaded " (count bytes) " bytes" (zc/element-id-str zc/*current-owner*))
              (zc/set-state! this {:state :loaded :data bytes})))
          (catch Exception e
            (log/error e))))))
  :render (fn [this]
    {:post [(zc/element? %)]}
    (let [{:keys [src render]} (zc/props this)
          {:keys [state data]} (zc/state this)]
      (log/trace "File Resource render" src state)
      (case state
         :not-loaded
           (zc/csx [:text {} ["not loaded"]])
         :loading
           (zc/csx [:text {} ["loading"]])
         :loaded
           (render data))))}))

(def UrlResource (zc/create-react-class {
  :display-name "UrlResource"
  :get-initial-state (fn [] {:state :not-loaded})
  :component-will-mount (fn [this]
    (log/info "UrlResource component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [src child-type]} (zc/props this)]
      #_(zc/set-state! this {:state :loading})
      (future
        (try
          (binding [zc/*current-owner* owner
                    zc/*updater* updater]
            (log/info "UrlResource component-will-mount load-fn" (zc/element-id-str owner))
            (let [bytes (get (swap! resource-cache
                                    cache/through-cache
                                    src
                                    (constantly (client/get src {:as :byte-array})))
                             src)]
              (zc/set-state! this {:state :loaded :data bytes})))
          (catch Exception e
            (log/error e)
            (assert false))))))
  :render (fn [this]
    (let [{:keys [src render]} (zc/props this)
          {:keys [state data]} (zc/state this)]
      (case state
         :not-loaded
           (zc/csx [:text {} ["not loaded"]])
         :loading
           (zc/csx [:text {} ["loading"]])
         :loaded
           (render data))))}))


(def Resource (zc/create-react-class {
  :display-name "Resource"
  :render (fn [this]
    (let [{:keys [src render]} (zc/props this)]
      (log/trace "Resource render" src)
      (if (clojure.string/starts-with? src "http")
        (zc/csx [UrlResource {:src src :render render}])
        (zc/csx [FileResource {:src src :render render}]))))}))
  
(def NativeImage (zc/create-react-class {
  :display-name "NativeImage"
  :get-initial-state (fn []
                       (log/info "NativeImage get-initial-state")
                       {:state :not-loaded})
  :component-will-mount (fn [this]
    (log/info "NativeImage component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [src child-type]} (zc/props this)]
      #_(zc/set-state! this {:state :loading})
      (future
        (try
          (binding [zc/*current-owner* owner
                    zc/*updater* updater]
            (log/info "NativeImage component-will-mount load-fn" (zc/element-id-str owner))
            (let [{:keys [data style]} (zc/props this)
                  clip (get style :clip)
                  k [data clip]
                  child (try
                          (get (swap! image-cache
                                    cache/through-cache
                                    k
                                    (constantly 
                                      (let [img (ziu/load-image data)
                                            clipped-img (if clip
                                                          (ziu/clip-image img clip)
                                                          img)
                                            pixels (ziu/pixel-seq clipped-img)
                                            width (ziu/width clipped-img)
                                            height (ziu/height clipped-img)
                                            lines (partition width pixels)]
                                        ; TODO: close?
                                        ;(.close img)
                                        ;(.close clipped-img)
                                        (log/info "NativeImage render" (get style :clip) width "x" height)
                                        #_(doseq [line lines]
                                          (log/info (vec line)))
                                        (let [img-characters (mapv (fn [[line1 line2]]
                                                        (mapv (fn [px1 px2]
                                                               {:c \u2580
                                                                :fg (conj (mapv (partial bit-and 0xFF) px1) 255)
                                                                :bg (conj (mapv (partial bit-and 0xFF) px2) 255)}) ; ▀
                                                             line1 line2))
                                                      (partition 2 lines))]
                                          (log/trace "img-characters" img-characters)
                                          (zc/csx [:img {:width width :height (/ height 2)}
                                                   img-characters])))))
                             k)
                            (catch Exception e
                              (log/error e)
                              (assert false)))]
              (zc/set-state! this {:state :loaded :child child})))
          (catch Exception e
            (log/error e)
            (assert false))))))
  :render (fn [this]
    (let [{:keys [src render]} (zc/props this)
          {:keys [state child]} (zc/state this)]
      (case state
         :not-loaded
           (zc/csx [:text {} ["not loaded"]])
         :loading
           (zc/csx [:text {} ["loading"]])
         :loaded
           child)))}))

(def RexPaintImage (zc/create-react-class {
  :display-name "RexPaintImage"
  :render (fn [this]
    (let [{:keys [data layer-index]} (zc/props this)
          layer-index (or layer-index 0)
          layers (rockpick.core/read-xp (clojure.java.io/input-stream data))
          layer (nth layers layer-index)
          pixels (mapv (fn [line]
               (log/info "line" line)
               (mapv (fn [{:keys [ch fg bg]}]
                      (let [fg-r (get fg :r)
                            fg-g (get fg :g)
                            fg-b (get fg :b)
                            fg-a 255
                            bg-r (get bg :r)
                            bg-g (get bg :g)
                            bg-b (get bg :b)
                            bg-a 255]
                      {:c (get zfont/cp437-unicode (int ch))
                       :fg [fg-r fg-g fg-b fg-a]
                       :bg [bg-r bg-g bg-b bg-a]}))
                     line))
               layer)]
      (log/info "pixels" pixels)
      (zc/csx [:img {:width (count (first pixels)) :height (count pixels)} pixels])))}))

(def DataImage  (zc/create-react-class {
  :display-name "DataImage"
  :render (fn [this]
    (let [{:keys [src data style]} (zc/props this)]
      (if (clojure.string/ends-with? src "xp")
        (zc/csx [RexPaintImage {:data data :style style}])
        (zc/csx [NativeImage {:data data :style style}]))))}))
  
(def Image (zc/create-react-class {
  :display-name "Image"
  :render (fn [this]
    (let [props (assoc (zc/props this)
                  :render (fn [data]
                            {:post [(zc/element? %)]}
                            (let [props (assoc (zc/props this) :data data)]
                              (log/trace "Image render-prop fn" (get props :src))
                              (zc/csx [DataImage props]))))]
      (log/trace "Image render" (get props :src))
      ;; passes :src :style, etc through
      (zc/csx [Resource props []])))}))

;; style taken from https://www.nucleo.com.au/using-flexbox-for-modal-dialogs/
(def Popup (zc/create-react-class {
  :display-name "Popup"
  :render (fn [this]
    (log/trace "Popup render")
    (let [{:keys [children] :as props} (zc/props this)]
      (zc/csx [:view {:style {:max-height "100%" :max-width "100%"
                              :height "100%"
                              :align-items :center
                              :justify-content :center
                              :position :fixed
                              :top 0 :left 0
                              :fg [0 0 0 128]
                              :bg [0 0 0 128]}} [
                [:view {:style {:bg [0 0 0 255]
                                :fg [255 255 255 255]
                                :margin-top 10
                                ;:margin-bottom 10
                                :padding 0
                                :border 1 :border-style :single
                                :text-align :center
                                :max-height "90%"
                                :max-width "90%"}}
                    children]]])))}))

(def AnimateProps (zc/create-react-class {
  :display-name "AnimateProps"
  :get-initial-state (fn []
                       (log/debug "AnimateProps get-initial-state")
                       {:t 0
                        :start-time (System/currentTimeMillis)})
  :render (fn [this]
    (log/debug "AnimateProps render")
    (let [{:keys [children] :as props} (zc/props this)
          {:keys [start-time]} (zc/state this)
          generators (dissoc props :children)
          t (- (System/currentTimeMillis) start-time)
          child-props (clojure.walk/postwalk (fn [node]
                                               (cond
                                                 (fn? node)
                                                   (try
                                                     (node t)
                                                     (catch Exception e
                                                       (log/error e)
                                                       (assert false e)))
                                                 :else
                                                   node))
                                             generators)
          updated-children (map (fn [child]
                                  (update child :props 
                                    (fn [prev-child-props]
                                      (zc/deep-merge prev-child-props child-props))))
                                children)]
      (first updated-children)))}))

(defn interpolate-to [start end]
  (fn [steps]
    (let [interval (- end start)
          step (/ interval steps)]
      (range start end step))))

(defn cycle [open-chan chan-fn]
  (fn []
    (go-loop []
       (<! (chan-fn))
       (when (= :open (<! open-chan))
         (recur)))))

(defn parallel [& chans]
  (fn []
    (go
      (<! (merge (map (fn [c] (c)) chans))))))

(defn keys-to-leaves [prefix m]
  (mapcat (fn [[k v]]
            (if (map? v)
              (keys-to-leaves (conj prefix k) v)
              [[(conj prefix k) v]]))
          m))

(defn transpose [xs]
  (apply map list xs))

(defmacro sequence [state-chan & cmds]
  `(fn [] (go
     ~@(map (fn [cmd next-cmd]
              (cond
                (number? cmd)
                  ;; numbers are pauses
                  `(<! (timeout ~cmd))
                (and (map? cmd) (number? next-cmd))
                  ;; maps are prop generators
                  (let [generators (keys-to-leaves [] cmd)
                        dt 33
                        duration (if (number? next-cmd) next-cmd 1)
                        steps (int (/ duration dt))
                        events (transpose
                                 (map (fn [[ks g]]
                                        (map (fn [v] [ks v])
                                             (if (list? g)
                                               ((eval g) steps)
                                               (repeat steps g))))
                                      generators))
                        state-changes (map (fn [event]
                                             (reduce (fn [m [ks v]]
                                                       `(>! ~state-chan ~(assoc-in m ks v)))
                                                     {}
                                                     event))
                                             events)
                        change-cmds `(go ~@(interpose `(<! (timeout ~dt)) state-changes))]
                    change-cmds)
                  :default
                     (assert false (str "cmd must be map or number. found:" cmd " " next-cmd))))
            cmds
            (concat (rest cmds) [(first cmds)])))))
  
(def AnimateProps2 (zc/create-react-class {
  :display-name "AnimateProps2"
  :get-initial-state (fn []
                       (log/debug "AnimateProps2 get-initial-state")
                       {:open-chan (to-chan (repeat :open))
                        :start-time (System/currentTimeMillis)})
  :component-will-mount (fn [this]
    (log/info "AnimateProps2 component-will-mount" (zc/element-id-str zc/*current-owner*))
    ;; pass current-owner binding through to the scheduled fn
    (let [owner zc/*current-owner*
          updater zc/*updater*
          {:keys [gen]} (zc/props this)
          {:keys [open-chan]} (zc/state this)
          state-chan (chan)]
      ;; receives state changes through state-chan
      (go-loop []
         (binding [zc/*current-owner* owner
                   zc/*updater* updater]
           (let [m (<! state-chan)]
             (log/info "AnimateProps2 receive state " m)
             (zc/set-state! this {:child-props m})))
          (recur))
      ;; puts state changes on state-chan until the open-chan closes
      ((gen state-chan open-chan))))
  :render (fn [this]
    (log/debug "AnimateProps2 render")
    (let [{:keys [children] :as props} (zc/props this)
          {:keys [child-props
                  state-chan
                  open-chan]} (zc/state this)
          updated-children (map (fn [child]
                                  (update child :props 
                                    (fn [prev-child-props]
                                      (zc/deep-merge prev-child-props child-props))))
                                children)]
      (first updated-children)))}))

