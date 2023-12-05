;; Functions for rendering state to screen
(ns zaffre.terminal
  (:require
   [zaffre.color :as zcolor]
   [taoensso.timbre :as log])
  (:import
    (java.nio ByteBuffer)
    (java.util Arrays)
    (org.lwjgl.glfw GLFW)))

(defn blend-mode->byte [blend-mode]
  (blend-mode {
    :normal 0x0
    :multiply 0x1
    :screen 0x2
    :overlay 0x3
    :darken 0x4
    :lighten 0x5
    :color-dodge 0x6
    :color-burn 0x7
    :hard-light 0x8
    :soft-light 0x9
    :difference 0x10
    :exclusion 0x11
    ;:hue 0x12
    ;:saturation 0x13
    ;:color 0x14
    ;:luminosity 0x15
    }
    #_(assert false (str "Unknown blend-mode " blend-mode))))

(defprotocol IBuffer
  (num-cols [this])
  (num-rows [this])
  (character-buffer [this])
  (blend-mode-buffer [this])
  (palette-index-buffer [this])
  (fg-buffer [this])
  (bg-buffer [this])
  (get-char [this col row])
  (get-blend-mode [this col row])
  (get-palette-index [this col row])
  (get-fg [this col row])
  (get-bg [this col row])
  (set! [this c blend-mode palette-index fg bg col row])
  (copy-fg [this offset-col offset-row length dest])
  (copy-bg [this offset-col offset-row length dest])
  (zero! [this]))

(defrecord Buffers [^int num-cols ^int num-rows
                    ^"[C" character-buffer
                    ^"[I" blend-mode-buffer
                    ^"[I" palette-index-buffer
                    ^"[I" fg-buffer
                    ^"[I" bg-buffer]
  IBuffer
  (num-cols [this] num-cols)
  (num-rows [this] num-rows)
  (character-buffer [this] character-buffer)
  (blend-mode-buffer [this] blend-mode-buffer)
  (palette-index-buffer [this] palette-index-buffer)
  (fg-buffer [this] fg-buffer)
  (bg-buffer [this] bg-buffer)
  (get-char [this col row]
    (let [col (int col)
          row (int row)
          index (+ (* row num-cols) col)]
      (aget character-buffer index)))
  (get-blend-mode [this col row]
    (let [col (int col)
          row (int row)
          index (+ (* row num-cols) col)]
      (aget blend-mode-buffer index)))
  (get-palette-index [this col row]
    (let [col (int col)
          row (int row)
          index (+ (* row num-cols) col)]
      (aget palette-index-buffer index)))
  (get-fg [this col row]
    (let [col (int col)
          row (int row)
          index (+ (* row num-cols) col)]
      (aget fg-buffer index)))
  (get-bg [this col row]
    (let [col (int col)
          row (int row)
          index (+ (* row num-cols) col)]
      (aget bg-buffer index)))
  (set! [this c blend-mode palette-index fg bg col row]
    (let [col (int col)
          row (int row)]
      (when (and (< -1 col) (< col num-cols)
                 (< -1 row) (< row num-rows))
        (let [index (+ (* row num-cols) col)
              fg-rgba (cond
                           (integer? fg) fg
                           (vector? fg) (zcolor/color fg))
              bg-rgba (cond
                           (integer? bg) bg
                           (vector? bg) (zcolor/color bg))]
          (aset character-buffer index (unchecked-char c))
          (aset blend-mode-buffer index (unchecked-int (blend-mode->byte blend-mode)))
          (aset palette-index-buffer index (unchecked-int palette-index))
          (aset fg-buffer index (unchecked-int fg-rgba))
          (aset bg-buffer index (unchecked-int bg-rgba))))))
  (copy-fg [this offset-col offset-row length dest]
    (let [^ByteBuffer dest dest
          offset (+ (* offset-row num-cols) offset-col)]
      (.put (.asIntBuffer dest) fg-buffer offset length)))
  (copy-bg [this offset-col offset-row length dest]
    (let [^ByteBuffer dest dest
          offset (+ (* offset-row num-cols) offset-col)]
      (.put (.asIntBuffer dest) bg-buffer offset length)))
  (zero! [this]
    (Arrays/fill character-buffer (char 0))
    (Arrays/fill blend-mode-buffer (int 0))
    (Arrays/fill palette-index-buffer (int 0))
    (Arrays/fill fg-buffer (int 0))
    (Arrays/fill bg-buffer (int 0))))
      

(defn array-buffers [num-cols num-rows]
  (->Buffers
    num-cols
    num-rows
    (char-array (* num-cols num-rows) \space)
    (int-array (* num-cols num-rows))
    (int-array (* num-cols num-rows))
    (int-array (* num-cols num-rows))
    (int-array (* num-cols num-rows))))

(defprotocol Terminal
  "Methods suffixed with ! indicate a change of state within the terminal. refresh! and destroy! are not transaction-safe and must not be called from within a transaction."
  (args [this] "Returns the option arguments passed to create-terminal.")
  (groups [this] "Returns the groups argument passed to create-terminal.")
  (alter-group-pos! [this group-id pos-fn] "Change the [x y] position of a layer group. `x` and `y` are mesured in pixels from the upper left corner of the screen.")
  (alter-group-font! [this group-id font-fn] "Changes the font for a layer group. `font-fn` is a function that takes one argument: one of :linux :macosx or :windows, and returns a font.")
  (put-chars! [this layer-id characters] "Changes the characters in a layer. `characters` is a sequence where each element is a map and must have these keys: :c - a character or keyword, :x int, column, :y int row, :fg [r g b], :bg [r g b] where r,g,b are ints from 0-255.")
  (put-layer! [this layer-id buffers] "Replaces all the characters in a layer. `buffer is an object implementing IBuffer.")
  (assoc-shader-param! [this k v] "Changes the value of a uniform variable in the post-processing shader.")
  (pub [this] "Returns a clojure.core.async publication partitioned into these topics:
              :keypress
              :mouse-down
              :mouse-up 
              :click
              :mouse-leave
              :mouse-enter
              :clipboard
              :drag-and-drop
              :close.")
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
