;; Functions for rendering state to screen
(ns zaffre.terminal
  (:require
   [taoensso.timbre :as log]))


(defprotocol Terminal
  "Methods suffixed with ! indicate a change of state within the terminal. refresh! and destroy! are not transaction-safe and must not be called from within a transaction."
  (args [this] "Returns the option arguments passed to create-terminal.")
  (groups [this] "Returns the groups argument passed to create-terminal.")
  (alter-group-pos! [this group-id pos-fn] "Change the [x y] position of a layer group. `x` and `y` are mesured in pixels from the upper left corner of the screen.")
  (alter-group-font! [this group-id font-fn] "Changes the font for a layer group. `font-fn` is a function that takes one argument: one of :linux :macosx or :windows, and returns a font.")
  (put-chars! [this layer-id characters] "Changes the characters in a layer. `characters` is a sequence where each element is a map and must have these keys: :c - a character or keyword, :x int, column, :y int row, :fg [r g b], :bg [r g b] where r,g,b are ints from 0-255.")
  (replace-chars! [this layer-id characters] "Replaces the characters in a layer. `characters is an array of arrays where each element is a map and must have these keys: :c - character or keyword, :fg [r g b] :bg [r g b] where r,g,b are ints from 0-355.")
  (set-fg! [this layer-id x y fg] "Changes the foreground color of a character in a layer.")
  (set-bg! [this layer-id x y bg] "Changes the background color of a character in a layer.")
  (assoc-shader-param! [this k v] "Changes the value of a uniform variable in the post-processing shader.")
  (pub [this] "Returns a clojure.core.async publication partitioned into these topics: :keypress :mouse-down :mouse-up :click :mouse-leave :mouse-enter :close.")
  (refresh! [this] "Uses group and layer information to draw to the screen.")
  (clear! [this]
          [this layer-id] "Clears all layers or just a specific layer.")
  (set-window-size! [this v] "Changes the terminal to fullscreen mode if v is a value returned by fullscreen-sizes. If false is supplied the terminal will revert to windowed mode.")
  (fullscreen-sizes [this] "Returns a list of fullscreen values.")
  (destroy! [this] "Stops the terminal, and closes the window.")
  (destroyed? [this] "True if destroy! cas been called or the window closed."))

(defmulti do-frame-clear type)

(defmacro time-val
  "Evaluates expr and returns the time it took.  Returns the value of
 [expr millis]."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         dt# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     [ret# dt#]))

(defmacro do-frame
  ([t d & body]
    `(let [terminal# ~t
           sleep-time# ~d]
       (future
         (-> (Thread/currentThread) (.setName (str "render-thread-" sleep-time#)))
         (try
           (loop []
             (when-not (destroyed? terminal#)
               (let [dt# (second
                           (time-val
                             (dosync
							   (do-frame-clear terminal#)
							   ~@body
							   (refresh! terminal#))))
                     pause# (max 0 (- sleep-time# dt#))]
               (when (< 0 pause#)
                 (Thread/sleep pause#))
               (recur))))
           (log/info "finished do-frame loop")
           (catch Throwable th#
             (log/error th# "Error rendering")))))))

;; namespace with only a protocol gets optimized out, causing missing dependencies.
;; add a dummp def to prevent this ns from being optimized away.
#?(:cljs
(def x 1))
