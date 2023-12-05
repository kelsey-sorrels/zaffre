; Functions for rendering characters to screen
(ns zaffre.glterminal
  (:require [zaffre.aterminal :as zat]
            [zaffre.util :as zutil]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure-watch.core :as cwc]
            [clojure.core.async :as async])
  (:import
    (java.lang.reflect Field)
    (java.awt Canvas
              Color
              Font
              FontMetrics
              Graphics
              Graphics2D
              RenderingHints)
    (org.lwjgl BufferUtils LWJGLUtil)
    (java.nio FloatBuffer ByteBuffer)
    (java.nio.charset Charset)
    (org.lwjgl.opengl Display ContextAttribs
                      PixelFormat DisplayMode Util
                      GL11 GL12 GL13 GL15 GL20 GL30 GL32)
    (org.lwjgl.input Keyboard Mouse)
    (org.lwjgl.util.vector Matrix4f Vector3f)
    (de.matthiasmann.twl.utils PNGDecoder PNGDecoder$Format)
    (java.io File FileInputStream FileOutputStream)
    (java.awt.image BufferedImage DataBufferByte)
    (javax.imageio ImageIO)
    (zaffre.aterminal ATerminal))
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro with-gl-context
  "Executes exprs in an implicit do, while holding the monitor of x and aquiring/releasing the OpenGL context.
  Will release the monitor of x in all circumstances."
  [x & body]
  `(let [lockee# ~x]
     (try
       (monitor-enter lockee#)
       (when @lockee#
         (Display/makeCurrent)
         ~@body)
       (finally
         (when @lockee#
           (Display/releaseContext))
         (monitor-exit lockee#)))))

(defmacro defn-memoized [fn-name & body]
  "Def's a memoized fn. Same semantics as defn."
  `(def ~fn-name (memoize (fn ~@body))))

(defmacro loop-with-index [idx-name bindings & body]
  "bindings => binding-form seq-expression

  Repeatedly executes body (presumably for side-effects) with
  binding form and idx-name in scope.  `idx-name` is repeatedly bound to the index of the item being
  evaluated ex: to each successive value in `(range (count (second bindings)))`. Does not retain
      the head of the sequence. Returns nil.

   (loop-with-index idx [[k v] {:a 1 :b 2 :c 3}] (println \"idx\" idx \"k\" k \"v\" v))
   idx 0 k :a v 1
   idx 1 k :b v 2
   idx 2 k :c v 3
   nil"
  (let [form (bindings 0) coll (bindings 1)]
     `(loop [coll# ~coll
             ~idx-name 0]
        (when coll#
          (let [~form (first coll#)]
            ~@body
            (recur (next coll#) (inc ~idx-name)))))))

(defn convert-key-code [event-char event-key on-key-fn]
  ;; Cond instead of case. For an unknown reason, case does not match event-key to Keyboard/* constants.
  ;; Instead it always drops to the default case
  (when-let [key (condp = (int event-key)
                   Keyboard/KEY_RETURN  :enter
                   Keyboard/KEY_ESCAPE  :escape
                   Keyboard/KEY_SPACE   :space
                   (int Keyboard/KEY_BACK)    :backspace
                   Keyboard/KEY_TAB     :tab
                   Keyboard/KEY_F1      :f1
                   Keyboard/KEY_F2      :f2
                   Keyboard/KEY_F3      :f3
                   Keyboard/KEY_F4      :f4
                   Keyboard/KEY_F5      :f5
                   Keyboard/KEY_F6      :f6
                   Keyboard/KEY_F7      :f7
                   Keyboard/KEY_F8      :f8
                   Keyboard/KEY_F9      :f9
                   Keyboard/KEY_F10     :f10
                   Keyboard/KEY_F11     :f11
                   Keyboard/KEY_F12     :f12
                   Keyboard/KEY_UP      :up
                   Keyboard/KEY_DOWN    :down
                   Keyboard/KEY_LEFT    :left
                   Keyboard/KEY_RIGHT   :right
                   Keyboard/KEY_NUMPAD1 :numpad1
                   Keyboard/KEY_NUMPAD2 :numpad2
                   Keyboard/KEY_NUMPAD3 :numpad3
                   Keyboard/KEY_NUMPAD4 :numpad4
                   Keyboard/KEY_NUMPAD5 :numpad5
                   Keyboard/KEY_NUMPAD6 :numpad6
                   Keyboard/KEY_NUMPAD7 :numpad7
                   Keyboard/KEY_NUMPAD8 :numpad8
                   Keyboard/KEY_NUMPAD9 :numpad9
                   ;; event-key didn't match, default to event-char if it is printable, else nil
                   (if (<= (int (first " ")) (int event-char) (int \~))
                     event-char
                     nil))]
    (log/info "key" key)
    (on-key-fn key)))

(defn font-key [^Font font] [(.getName font) (.getSize font)])

(defn make-font
  [name-or-path style size]
  (let [font-file ^File (clojure.java.io/as-file name-or-path)]
    (if (.exists font-file)
      ;; Load font from file
      (do
        (log/info "Loading font from file" name-or-path)
        (.deriveFont (Font/createFont Font/TRUETYPE_FONT font-file) (int style) (float size)))
      ;; Load font from font registry
      (do
        (log/info "Loading font by name")
        (Font. name-or-path style size)))))

(defn next-pow-2 [v]
  (int (Math/pow 2 (inc (Math/floor (/ (Math/log v) (Math/log 2)))))))


(def cjk-blocks
  (set
    (concat (range 0x2E80 0x2EFF)
            (range 0x3000 0x9FFF)
            (range 0xAC00 0xD7AF)
            (range 0xF900 0xFAFF))))


;; A sequence of [character underline?]
(defn displayable-characters [^Font font]
  (let [chars (map char (filter (fn [c] (and (.canDisplay font (char c))
                                             (not (contains? cjk-blocks c))))
                                (range 0x0000 0xFFFF)))]
    (concat (map vector chars (repeat false))
            (map vector chars (repeat true)))))

(defn characters-per-line [char-width num-characters]
  (int (Math/floor (/ (Math/pow 2 11) char-width))))

;; A sequence of [[\character underline?] x y] where [x y] is the column,row in the character atlas.
(defn character-idxs
  [char-width characters]
  ;(let [character-matrix (partition-all (int (inc (Math/sqrt (count characters)))) characters)]
  (let [character-matrix    (partition-all (characters-per-line char-width (count characters)) characters)]
    (log/info "arranging character with" characters-per-line "per line")
    (mapcat concat
    (map-indexed (fn [row line]
                   (map-indexed (fn [col c-underline?]
                                  [c-underline? col row])
                                line))
                 character-matrix))))


;; Map [\character underline?] to [col row]
(defn-memoized character->col-row
  [character-idxs]
  (reduce (fn [m [c-underline? x y]]
            (assoc m c-underline? [x y]))
          {}
          character-idxs))

(defn make-glyph-image
  [char-width char-height font]
  ;; Adjust canvas to fit character atlas size
  (let [characters (displayable-characters font)
        _ (log/info "characters per line" (characters-per-line char-width (count characters)))
        cwidth     2048
        cheight    (next-pow-2 (* char-height (int (Math/ceil (/ (count characters)
                                                                 (characters-per-line char-width (count characters)))))))
        width      cwidth ;(max cwidth cheight)
        height     cheight ;(max cwidth cheight)
        antialias  true
        font-metrics  ^FontMetrics (.getFontMetrics (Canvas.) font)]
    (log/info "characters" (count characters) "cwidth" cwidth "cheight" cheight)
    (log/info "glyph image width" width "height" height)
    ;(log/info "characters" (vec characters))
    ;(log/info "character-idxs" (vec (character-idxs characters)))
    (let [texture-image    (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR)
          texture-graphics ^Graphics2D (.getGraphics texture-image)
          white            (Color. 255 255 255 255)]
      ;; Create and clear graphics
      (doto texture-graphics
        (.setFont font)
        (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING (if antialias
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_GASP
                                                                  RenderingHints/VALUE_TEXT_ANTIALIAS_OFF))
        ;; set background to black
        (.setColor (Color. 0 0 0 0))
        (.fillRect 0 0 width height))
      ;; Loop through each character, drawing it
      (doseq [[[s underline?] col row]  (character-idxs char-width characters)]
        (let [x ^int (* col char-width)
              y ^int (* (inc row) char-height)
              cx (+ 0 x)
              cy (- y (.getDescent font-metrics))]
          ;(log/info s x y)
        (when (not= s " ")
          ;(println "drawing" s "@" x y "underline?" underline?)
          (doto texture-graphics
            (.setColor white)
            (.setClip cx (+ (- cy char-height) (.getDescent font-metrics)) char-width char-height)
            (.drawString (str s) cx cy)))
        (when underline?
          (let [y (dec y)]
            (doto texture-graphics
              (.setColor white)
              (.drawLine x
                         y
                         (+ x char-width)
                         y))))))
      ;; cleanup texture resource
      ;(ImageIO/write texture-image "jpg", (File. "glyph-texture.jpg"))
      (.dispose texture-graphics)
      {:font-texture-width width
       :font-texture-height height
       :font-texture-image texture-image})))


(defn- buffered-image-byte-buffer [^BufferedImage buffered-image]
  (let [width          (.getWidth buffered-image)
        height         (.getHeight buffered-image)
        texture-buffer (BufferUtils/createByteBuffer (* width height 4))
        data ^bytes (-> buffered-image
                 (.getRaster)
                 (.getDataBuffer)
                 (as-> db
                   (let [dbb ^DataBufferByte db]
                     (.getData dbb))))]
    (.put texture-buffer data 0 (alength data))
    (.flip texture-buffer)
    texture-buffer))

(defn- get-fields [#^Class static-class]
  (. static-class getFields))

(defn- gl-enum-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (.getName #^Field (some
                       #(if (= enum-value (.get #^Field % nil)) % nil)
                       (mapcat get-fields [GL11 GL12 GL13 GL15 GL20 GL30])))))

(defn- except-gl-errors
  [msg]
  (let [error (GL11/glGetError)
        error-string (str "OpenGL Error(" error "):"
                          (gl-enum-name error) ": " msg " - "
                          (Util/translateGLErrorString error))]
    (if (not (zero? error))
      (throw (Exception. error-string)))))

(defn- texture-id
  ([^BufferedImage buffered-image]
  (let [width (.getWidth buffered-image)
        height (.getHeight buffered-image)]
    (texture-id width height (buffered-image-byte-buffer buffered-image))))
  ([width height]
   (texture-id width height (BufferUtils/createByteBuffer (* width height 4))))
  ([^long width ^long height ^ByteBuffer texture-buffer]
   (let [texture-id (GL11/glGenTextures)]
     ;;(.order texture-buffer (ByteOrder/nativeOrder))
     (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
     (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
     (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
     (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
     (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE texture-buffer)
     (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
     texture-id)))

(defn- xy-texture-id [^long width ^long height ^ByteBuffer texture-buffer]
  (let [texture-id (GL11/glGenTextures)]
    ;;(.order texture-buffer (ByteOrder/nativeOrder))
    (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL30/GL_RGBA8UI width height 0 GL30/GL_RGBA_INTEGER GL11/GL_INT texture-buffer)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
    texture-id))


(defn- fbo-texture [^long width ^long height]
  (let [id (GL30/glGenFramebuffers)]
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER id)
    (let [texture-id (GL11/glGenTextures)
          draw-buffer (BufferUtils/createIntBuffer 1)
          ;; type nil as ByteBuffer to avoid reflection on glTexImage2D
          ^ByteBuffer bbnil nil]
      (.put draw-buffer GL30/GL_COLOR_ATTACHMENT0)
      (.flip draw-buffer)
      ;;(.order texture-buffer (ByteOrder/nativeOrder))
      (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
      (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB width height 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE bbnil)
      (GL32/glFramebufferTexture GL30/GL_FRAMEBUFFER GL30/GL_COLOR_ATTACHMENT0 texture-id 0)
      (GL20/glDrawBuffers draw-buffer)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
      (except-gl-errors "end of fbo-texture")
      (when (not= (GL30/glCheckFramebufferStatus GL30/GL_FRAMEBUFFER) GL30/GL_FRAMEBUFFER_COMPLETE)
        (throw (Exception. "Framebuffer not complete")))
      [id texture-id])))
        
(defn png-bytes [path]
  (let [input-stream (FileInputStream. (str path))
        decoder (PNGDecoder. input-stream)
        width (.getWidth decoder)
        height (.getHeight decoder)
        bytebuf (ByteBuffer/allocateDirect (* width height 4))]
    (.decode decoder bytebuf (* width 4) PNGDecoder$Format/RGBA)
    (.flip bytebuf)
    (.close input-stream)
    bytebuf))

;; Extract native libs and setup system properties
(defn init-natives []
  (when (.exists (File. "natives"))
  ;(System/setProperty "java.library.path", (.getAbsolutePath (File. "natives")))
  (condp = [(LWJGLUtil/getPlatform) (.endsWith (System/getProperty "os.arch") "64")]
    [LWJGLUtil/PLATFORM_LINUX false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/linux/x86")))
      [LWJGLUtil/PLATFORM_LINUX true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/linux/x86_64")))
    [LWJGLUtil/PLATFORM_MACOSX false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/macosx/x86")))
    [LWJGLUtil/PLATFORM_MACOSX true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/macosx/x86_64")))
    [LWJGLUtil/PLATFORM_WINDOWS false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/windows/x86")))
    [LWJGLUtil/PLATFORM_WINDOWS true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/windows/x86_64"))))))

(defn- init-display [title screen-width screen-height icon-paths gl-lock destroyed]
  (let [pixel-format       (PixelFormat.)
        context-attributes (doto (ContextAttribs. 3 2)
                             (.withForwardCompatible true)
                             (.withProfileCore true))
        icon-array         (when icon-paths
                             (condp = (LWJGLUtil/getPlatform)
                               LWJGLUtil/PLATFORM_LINUX (let [icon-array ^"[Ljava.nio.ByteBuffer;" (make-array ByteBuffer 1)
                                                              data ^bytes (png-bytes (get icon-paths 1))]
                                                          (aset icon-array 0 data)
                                                          icon-array)
                               LWJGLUtil/PLATFORM_MACOSX  (let [icon-array ^"[Ljava.nio.ByteBuffer;" (make-array ByteBuffer 1)
                                                                data ^bytes (png-bytes (get icon-paths 2))]
                                                            (aset icon-array 0 data)
                                                            icon-array)
                               LWJGLUtil/PLATFORM_WINDOWS (let [icon-array ^"[Ljava.nio.ByteBuffer;" (make-array ByteBuffer 2)
                                                                data0 ^bytes (png-bytes (get icon-paths 0))
                                                                data1 ^bytes (png-bytes (get icon-paths 1))]
                                                            (aset icon-array 0 data0)
                                                            (aset icon-array 1 data1)
                                                            icon-array)))
        latch              (java.util.concurrent.CountDownLatch. 1)]
     ;; init-natives must be called before the Display is created
     (init-natives)
     (future
       (Display/setDisplayMode (DisplayMode. screen-width screen-height))
       (Display/setTitle title)
       (when icon-array
         (Display/setIcon icon-array))
       (Display/create pixel-format context-attributes)
       (Keyboard/create)
       (Mouse/create)
       (log/info "byte-buffer" icon-array)
       (GL11/glViewport 0 0 screen-width screen-height)
       ;; Release the Display so that any thread can aquire it including this thread - the thread that
       ;; created it.j
       (Display/releaseContext)
       ;; Signal to parent that display has been created
       (.countDown latch)
       (loop []
         (if (with-gl-context gl-lock
               ; Process messages in the main thread rather than the input go-loop due to Windows only allowing
               ; input on the thread that created the window
               (Display/processMessages)
               ;; Close the display if the close window button has been clicked
               ;; or the gl-lock has been released programmatically (e.g. by destroy!)
               (or (Display/isCloseRequested) @destroyed))
           (do
             (log/info "Destroying display")
             (with-gl-context gl-lock
               (reset! gl-lock false)
               ;; TODO: Clean up textures and programs
               #_(let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id vao-id fbo-id]} :buffers
                      {:keys [font-texture glyph-texture fg-texture bg-texture fbo-texture]} :textures
                      program-id :program-id
                      fb-program-id :fb-program-id} gl]
                 (doseq [id [program-id fb-program-id]]
                   (GL20/glDeleteProgram id))
                 (doseq [id [font-texture glyph-texture fg-texture bg-texture fbo-texture]]
                   (GL11/glDeleteTexture id)))
               (Display/destroy))
             (log/info "Exiting"))
           (do
             (Thread/sleep 5)
             (recur)))))
     ;; Wait for Display to be created
     (.await latch)))

(defn- load-shader
  [^String shader-str ^Integer shader-type]
  (let [shader-id         (GL20/glCreateShader shader-type)
        _ (except-gl-errors "@ load-shader glCreateShader ")
        _                 (GL20/glShaderSource shader-id shader-str)
        _ (except-gl-errors "@ load-shader glShaderSource ")
        _                 (GL20/glCompileShader shader-id)
        _ (except-gl-errors "@ load-shader glCompileShader ")
        gl-compile-status (GL20/glGetShaderi shader-id GL20/GL_COMPILE_STATUS)
        _ (except-gl-errors "@ end of let load-shader")]
    (when (== gl-compile-status GL11/GL_FALSE)
      (println "ERROR: Loading a Shader:")
      (println (GL20/glGetShaderInfoLog shader-id 10000)))
    [gl-compile-status shader-id]))

(defn- program-id [vs-id fs-id & attribute-names]
  (let [pgm-id                (GL20/glCreateProgram)
        _ (except-gl-errors "@ let init-shaders glCreateProgram")
        _                     (GL20/glAttachShader pgm-id vs-id)
        _ (except-gl-errors "@ let init-shaders glAttachShader VS")
        _                     (GL20/glAttachShader pgm-id fs-id)
        _ (except-gl-errors "@ let init-shaders glAttachShader FS")
        _                     (GL20/glLinkProgram pgm-id)
        _ (except-gl-errors "@ let init-shaders glLinkProgram")
        gl-link-status        (GL20/glGetProgrami pgm-id GL20/GL_LINK_STATUS)
        _ (except-gl-errors "@ let init-shaders glGetProgram link status")
        _                     (when (== gl-link-status GL11/GL_FALSE)
                                (println "ERROR: Linking Shaders:")
                                (println (GL20/glGetProgramInfoLog pgm-id 10000)))
        _ (except-gl-errors "@ let before GetUniformLocation")
        ]
    (doseq [[^int i ^String attribute-name] (map-indexed vector attribute-names)]
      (GL20/glBindAttribLocation pgm-id i attribute-name))
    pgm-id))

(defn fx-shader-content [fx-shader-name]
  (slurp (or (clojure.java.io/resource fx-shader-name)
             fx-shader-name)))

(defn- init-shaders
  [fx-shader-name]
  (let [[ok? vs-id]    (load-shader (-> "shader.vs" clojure.java.io/resource slurp)  GL20/GL_VERTEX_SHADER)
        _              (assert (== ok? GL11/GL_TRUE)) ;; something is really wrong if our vs is bad
        [ok? fs-id]    (load-shader (-> "shader.fs" clojure.java.io/resource slurp) GL20/GL_FRAGMENT_SHADER)
        _              (assert (== ok? GL11/GL_TRUE)) ;; something is really wrong if our fs is bad
        [ok? fx-vs-id] (load-shader (-> "shader.vs" clojure.java.io/resource slurp) GL20/GL_VERTEX_SHADER)
        _              (assert (== ok? GL11/GL_TRUE)) ;; something is really wrong if our fs is bad
        [ok? fx-fs-id] (load-shader (fx-shader-content fx-shader-name) GL20/GL_FRAGMENT_SHADER)
        _              (assert (== ok? GL11/GL_TRUE)) ;; something is really wrong if our fs is bad
       ]
    (if (== ok? GL11/GL_TRUE)
      (let [pgm-id    (program-id vs-id fs-id "aVertexPosition" "aTextureCoord")
            fb-pgm-id (program-id fx-vs-id fx-fs-id "aVertexPosition" "aTextureCoord")]
        [pgm-id fb-pgm-id])
      (log/error "Error loading shaders"))))

(defn ortho-matrix-buffer
  ([viewport-width viewport-height]
    (ortho-matrix-buffer viewport-width viewport-height (BufferUtils/createFloatBuffer 16)))
  ([viewport-width viewport-height ^FloatBuffer matrix-buffer]
    (let [ortho-matrix (doto (Matrix4f.)
                         (.setIdentity))
          matrix-buffer matrix-buffer
          zNear   10
          zFar   -10
          m00     (/ 2 viewport-width)
          m11     (/ 2 viewport-height)
          m22     (/ -2 (- zFar zNear))
          m23     (/ (- (+ zFar zNear)) (- zFar zNear))
          m33     1]
      (set! (.m00 ortho-matrix) m00)
      (set! (.m11 ortho-matrix) m11)
      (set! (.m22 ortho-matrix) m22)
      (set! (.m23 ortho-matrix) m23)
      (set! (.m33 ortho-matrix) m33)
      (.store ortho-matrix matrix-buffer)
      (.flip matrix-buffer)
      matrix-buffer)))

(defn position-matrix-buffer
  ([v s]
   (position-matrix-buffer v s (BufferUtils/createFloatBuffer 16)))
  ([v s ^FloatBuffer matrix-buffer]
    (let [matrix (doto (Matrix4f.)
                         (.setIdentity))]
      (.translate matrix (Vector3f. (get v 0) (get v 1) (get v 2)))
      (.scale matrix (Vector3f. (get s 0) (get s 1) (get s 2)))
      (.store matrix matrix-buffer)
      (.flip matrix-buffer)
      matrix-buffer)))

(defn- init-buffers []
  (let [vertices              (float-array [1.0   1.0  0.0,
                                            0.0   1.0  0.0
                                            1.0,  0.0 0.0
                                            0.0   0.0 0.0])
        texture-coords        (float-array [1.0 1.0
                                            0.0 1.0
                                            1.0 0.0
                                            0.0 0.0])
        vertices-buffer       (-> (BufferUtils/createFloatBuffer (count vertices))
                                  (.put vertices)
                                  (.flip))
        texture-coords-buffer (-> (BufferUtils/createFloatBuffer (count texture-coords))
                                  (.put texture-coords)
                                  (.flip))
        vertices-count        (count vertices)
        texture-coords-count  (count texture-coords)
        vao-id                (GL30/glGenVertexArrays)
        _                     (GL30/glBindVertexArray vao-id)
        vertices-vbo-id       (GL15/glGenBuffers)
        _ (except-gl-errors "glGenBuffers vertices-vbo-id")
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vertices-vbo-id)
        _ (except-gl-errors "glBindBuffer vertices-vbo-id")
        _                     (GL15/glBufferData GL15/GL_ARRAY_BUFFER ^FloatBuffer vertices-buffer GL15/GL_STATIC_DRAW)
        _ (except-gl-errors "glBufferData vertices-vbo-id")
        _                     (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)
        _ (except-gl-errors "glVertexAttribPointer vertices-vbo-id")
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
        _ (except-gl-errors "glBindBuffer0 vertices-vbo-id")
        texture-coords-vbo-id (GL15/glGenBuffers)
        _ (except-gl-errors "glGenBuffers texture-coords-vbo-id")
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER texture-coords-vbo-id)
        _                     (GL15/glBufferData GL15/GL_ARRAY_BUFFER ^FloatBuffer texture-coords-buffer GL15/GL_STATIC_DRAW)
        _                     (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false 0 0)
        _                     (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)]
    (GL30/glBindVertexArray 0)
    (except-gl-errors "end of init-buffers")
    {:vertices-vbo-id vertices-vbo-id
     :vertices-count vertices-count
     :texture-coords-vbo-id texture-coords-vbo-id
     :texture-coords-count texture-coords-count
     :vao-id vao-id}))

;; Normally this would be a record, but until http://dev.clojure.org/jira/browse/CLJ-1224 is fixed
;; it is not performant to memoize records because hashCode values are not cached and are recalculated
;; each time.

(defrecord GLCharacter [character fg-color bg-color style fx-character fx-fg-color fg-bg-color]
  Object
  (toString [this]
    (pr-str this)))

(defn make-terminal-character
  ([character fg-color bg-color style]
   (make-terminal-character character fg-color bg-color style nil nil nil))
  ([character fg-color bg-color style fx-character fx-fg-color fg-bg-color]
   (GLCharacter. character fg-color bg-color style fx-character fx-fg-color fg-bg-color)))

(defrecord OpenGlTerminal [^int columns
                           ^int rows
                           ^int texture-columns
                           ^int texture-rows
                           font-textures
                           normal-font
                           fbo-texture
                           antialias
                           character-map-cleared
                           layers-character-map
                           layer-order
                           cursor-xy
                           fx-uniforms
                           gl
                           key-chan
                           gl-lock
                           destroyed]
  zat/ATerminal
  (get-size [_]
    [columns rows])
  ;; characters is a list of {:c \character :x col :y row :fg [r g b] :bg [r g b]}
  (put-chars! [_ layer-id characters]
    {:pre [(contains? (set layer-order) layer-id)
           (get layers-character-map layer-id)]}
    #_(log/info "characters" (str characters))
    (alter (get layers-character-map layer-id)
           (fn [cm]
             (reduce (fn [cm [row row-characters]]
                       (if (< -1 row rows)
                         (assoc cm
                           row
                           (persistent!
                             (reduce
                               (fn [line c]
                                 (if (< -1 (get c :x) columns)
                                   (let [fg        (get c :fg)
                                         bg        (get c :bg)
                                         fg-color  fg
                                         bg-color  bg
                                         character (make-terminal-character (get c :c) fg-color bg-color #{})]
                                     (assoc! line (get c :x) character))
                                   line))
                               (transient (get cm row))
                               row-characters)))
                         cm))
                     cm
                     (group-by :y characters))))
    #_(log/info "character-map" (str @character-map)))
  (set-fg! [_ layer-id x y fg]
    {:pre [(vector? fg)
           (= (count fg) 3)]}
      (alter (get layers-character-map layer-id)
             (fn [cm] (assoc-in cm [y x :fg-color] fg))))
  (set-bg! [_ layer-id x y bg]
    {:pre [(vector? bg)
           (= (count bg) 3)]}
      (alter (get layers-character-map)
             (fn [cm] (assoc-in cm [y x :bg-color] bg))))
  (assoc-fx-uniform! [_ k v]
    (-> fx-uniforms (get k) first (reset! v)))
  (get-key-chan [_]
    key-chan)
  (apply-font! [_ windows-font else-font size smooth]
    (with-gl-context gl-lock
      (reset! normal-font
              (if (= (LWJGLUtil/getPlatform) LWJGLUtil/PLATFORM_WINDOWS)
                (make-font windows-font Font/PLAIN size)
                (make-font else-font Font/PLAIN size)))
      (let [{:keys [screen-width
                    screen-height
                    character-width
                    character-height
                    font-texture-width
                    font-texture-height
                    font-texture-image]} (get @font-textures (font-key @normal-font))
            ;; type nil as ByteBuffer to avoid reflection on glTexImage2D
            ^ByteBuffer bbnil nil]
        (log/info "screen size" screen-width "x" screen-height)
        (try
          (Display/setDisplayMode (DisplayMode. screen-width screen-height))
          ;; resize FBO
          (GL11/glBindTexture GL11/GL_TEXTURE_2D fbo-texture)
          (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB (int screen-width) (int screen-height) 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE bbnil)
          (swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture (texture-id font-texture-image))))
          (catch Throwable t
            (log/error "Eror changing font" t))))
      (reset! antialias smooth)))
  (set-cursor! [_ x y]
    (reset! cursor-xy [x y]))
  (refresh! [_]
    (with-gl-context gl-lock
      (let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id vao-id fbo-id]} :buffers
             {:keys [font-texture glyph-texture fg-texture bg-texture fbo-texture]} :textures
             program-id :program-id
             fb-program-id :fb-program-id
             {:keys [u-MVMatrix u-PMatrix u-fb-MVMatrix u-fb-PMatrix u-font u-glyphs u-fg u-bg font-size term-dim font-tex-dim
                     font-texture-width font-texture-height glyph-tex-dim glyph-texture-width glyph-texture-height
                     u-fb]} :uniforms
             {:keys [^ByteBuffer glyph-image-data
                     ^ByteBuffer fg-image-data
                     ^ByteBuffer bg-image-data]} :data
             :keys [p-matrix-buffer mv-matrix-buffer character-width character-height character->col-row]} gl
            glyph-image-data glyph-image-data
            fg-image-data fg-image-data
            bg-image-data bg-image-data
            {:keys [screen-width
                    screen-height
                    character-width
                    character-height
                    font-texture-width
                    font-texture-height
                    font-texture]} (get @font-textures (font-key @normal-font))
            base-layer-id (first layer-order)]
        (assert (not (nil? font-texture-width)) "font-texture-width nil")
        (assert (not (nil? font-texture-height)) "font-texture-height")
        (assert (not (nil? font-texture)) "font-texture nil")
        ;; Update glyph texture in buffers
        (.clear glyph-image-data)
        (.clear fg-image-data)
        (.clear bg-image-data)
        (doseq [[layer-id character-map] layers-character-map]
        ;;  (doseq [[row line] (map-indexed vector (reverse @character-map))
        ;;          [col c]    (map-indexed vector line)]
            ;;(log/info "row" row "col" col "c" c)
        (loop-with-index row [line (reverse @character-map)]
          (loop-with-index col [c line]
            (let [chr        (char (or (get c :fx-character) (get c :character)))
                  chr        (if (and (= layer-id base-layer-id)
                                      (= chr (char 0)))
                                 \space
                                 chr)
                  highlight  (= @cursor-xy [col (- rows row 1)])
                  [fg-r fg-g fg-b] (if highlight
                                     (or (get c :fx-bg-color)  (get c :bg-color))
                                     (or (get c :fx-fg-color)  (get c :fg-color)))
                  [bg-r bg-g bg-b] (if highlight
                                     (or (get c :fx-fg-color)  (get c :fg-color))
                                     (or (get c :fx-bg-color)  (get c :bg-color)))
                  ;s         (str (get c :character))
                  style     (get c :style)
                  i         (* 4 (+ (* texture-columns row) col))
                  [x y]     (get character->col-row [chr (contains? style :underline)])]
              ;(log/info "Drawing at col row" col row "character from atlas col row" x y c "(index=" i ")")
              (when (zero? col)
                (.position glyph-image-data i)
                (.position fg-image-data i)
                (.position bg-image-data i))
              (if (or (= layer-id base-layer-id)
                      (not= chr (char 0)))
                (do
                  (assert (or (not (nil? x)) (not (nil? y))) (format "X/Y nil - glyph not found for character %s %s" (or (str chr) "nil") (or (format "%x" (int chr)) "nil")))
                  (.put glyph-image-data (unchecked-byte x))
                  (.put glyph-image-data (unchecked-byte y))
                  (.put glyph-image-data (unchecked-byte 0))
                  (.put glyph-image-data (unchecked-byte 0))
                  (.put fg-image-data    (unchecked-byte fg-r))
                  (.put fg-image-data    (unchecked-byte fg-g))
                  (.put fg-image-data    (unchecked-byte fg-b))
                  (.put fg-image-data    (unchecked-byte 0))
                  (.put bg-image-data    (unchecked-byte bg-r))
                  (.put bg-image-data    (unchecked-byte bg-g))
                  (.put bg-image-data    (unchecked-byte bg-b))
                  (.put bg-image-data    (unchecked-byte 0)))
                ;; not base layer and space ie empty, skip forward
                (do
                  (.position glyph-image-data (+ 4 (.position glyph-image-data)))
                  (.position fg-image-data (+ 4 (.position fg-image-data)))
                  (.position bg-image-data (+ 4 (.position bg-image-data)))))))
          (.rewind glyph-image-data)
          (.rewind fg-image-data)
          (.rewind bg-image-data)))
        (.position glyph-image-data (.limit glyph-image-data))
        (.position fg-image-data (.limit fg-image-data))
        (.position bg-image-data (.limit bg-image-data))
        (.flip glyph-image-data)
        (.flip fg-image-data)
        (.flip bg-image-data)
        (try
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER, fbo-id)
          (GL11/glViewport 0 0 screen-width screen-height)
          (except-gl-errors (str "glViewport " screen-width screen-height))
          (GL11/glClearColor 0.0 0.0 1.0 1.0)
          (except-gl-errors (str "glClearColor  " 0.0 0.0 1.0 1.0))
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (except-gl-errors (str "glClear  " (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT)))
          (GL20/glUseProgram program-id)
          (GL20/glUniformMatrix4 u-PMatrix false (ortho-matrix-buffer screen-width screen-height p-matrix-buffer))
          (except-gl-errors (str "u-PMatrix - glUniformMatrix4  " u-PMatrix))
          (GL20/glUniformMatrix4 u-MVMatrix false (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
                                                                          [screen-width screen-height 1.0]
                                                                          mv-matrix-buffer))
          (except-gl-errors (str "u-MVMatrix - glUniformMatrix4  " u-MVMatrix))
          ; Bind VAO
          (GL30/glBindVertexArray vao-id)
          (except-gl-errors (str "vao bind - glBindVertexArray " vao-id))
          ; Setup vertex buffer
          ;(GL15/glBindBuffer GL15/GL_ARRAY_BUFFER, vertices-vbo-id)
          (except-gl-errors (str "vbo bind - glBindBuffer " vertices-vbo-id))
          (GL20/glEnableVertexAttribArray 0);pos-vertex-attribute)
          (except-gl-errors "vbo bind - glEnableVertexAttribArray")
          ;;(GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)
          (except-gl-errors "vbo bind - glVertexAttribPointer")
          ; Setup uv buffer
          ;(GL15/glBindBuffer GL15/GL_ARRAY_BUFFER, texture-coords-vbo-id)
          (GL20/glEnableVertexAttribArray 1);texture-coords-vertex-attribute)
          ;;(GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false 0 0)
          (except-gl-errors "texture coords bind")
          ; Setup font texture
          (GL13/glActiveTexture GL13/GL_TEXTURE0)
          (except-gl-errors "font texture glActiveTexture")
          (GL11/glBindTexture GL11/GL_TEXTURE_2D font-texture)
          (except-gl-errors "font texture glBindTexture")
          (GL20/glUniform1i u-font 0)
          (except-gl-errors "font texture glUniform1i")
          ; Setup uniforms for glyph, fg, bg, textures
          (GL20/glUniform1i u-glyphs 1)
          (GL20/glUniform1i u-fg 2)
          (GL20/glUniform1i u-bg 3)
          (except-gl-errors "uniformli bind")
          (GL20/glUniform2f font-size, character-width character-height)
          (GL20/glUniform2f term-dim columns rows)
          (GL20/glUniform2f font-tex-dim font-texture-width font-texture-height)
          (GL20/glUniform2f glyph-tex-dim glyph-texture-width glyph-texture-height)
          (except-gl-errors "uniform2f bind")
          (except-gl-errors "gl(en/dis)able")
          ; Send updated glyph texture to gl
          (GL13/glActiveTexture GL13/GL_TEXTURE1)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D glyph-texture)
          (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL30/GL_RGBA8UI texture-columns texture-rows 0 GL30/GL_RGBA_INTEGER GL11/GL_UNSIGNED_BYTE glyph-image-data)
          (except-gl-errors "glyph texture data")
          ; Send updated fg texture to gl
          (GL13/glActiveTexture GL13/GL_TEXTURE2)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D fg-texture)
          (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA texture-columns texture-rows 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE fg-image-data)
          (except-gl-errors "fg color texture data")
          ; Send updated bg texture to gl
          (GL13/glActiveTexture GL13/GL_TEXTURE3)
          (except-gl-errors "bg color glActiveTexture")
          (GL11/glBindTexture GL11/GL_TEXTURE_2D bg-texture)
          (except-gl-errors "bg color glBindTexture")
          (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA texture-columns texture-rows 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bg-image-data)
          (except-gl-errors "bg color glTexImage2D")
          (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
          (except-gl-errors "bg color glDrawArrays")
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)


          ;; Draw fbo to screen
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
          (GL11/glViewport 0 0 screen-width screen-height)
          (GL11/glClearColor 1.0 0.0 0.0 1.0)
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (GL20/glUseProgram fb-program-id)
          (GL20/glUniformMatrix4 u-fb-PMatrix false (ortho-matrix-buffer screen-width screen-height p-matrix-buffer))
          (except-gl-errors (str "u-fb-PMatrix - glUniformMatrix4  " u-fb-PMatrix))
          (GL20/glUniformMatrix4 u-fb-MVMatrix false (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
                                                                          [screen-width screen-height 1.0]
                                                                          mv-matrix-buffer))
          (except-gl-errors (str "u-fb-MVMatrix - glUniformMatrix4  " u-fb-MVMatrix))
          (GL20/glEnableVertexAttribArray 0);pos-vertex-attribute)
          (except-gl-errors "vbo bind - glEnableVertexAttribArray")
          ;;; Setup uv buffer
          (GL20/glEnableVertexAttribArray 1);texture-coords-vertex-attribute)
          (except-gl-errors "texture coords bind")
          ;;; Setup font texture
          (GL13/glActiveTexture GL13/GL_TEXTURE0)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D fbo-texture)
          (GL20/glUniform1i u-fb 0)
          ;;; Set fx uniforms
          ;(log/info "fx-uniforms" (vec fx-uniforms))
          (doseq [[uniform-name [value-atom location]] fx-uniforms]
            (when (not (neg? location))
              (GL20/glUniform1f location (-> value-atom deref float))))
          ;; Draw fx shaded terminal
          (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
          ;;;; clean up
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)
          (GL30/glBindVertexArray 0)
          (except-gl-errors "end of refresh")
          ;(Display/sync 60)
          (Display/update false)
          (except-gl-errors "end of update")
          (catch Error e
            (log/error "OpenGL error:" e))))))
  (clear! [_]
    (doseq [[_ character-map] layers-character-map]
      (ref-set character-map character-map-cleared)))
  (clear! [_ layer-id]
    {:pre [(contains? (set layer-order) layer-id)]}
    (ref-set (get layers-character-map layer-id) character-map-cleared))
  (set-fx-fg! [_ layer-id x y fg]
    {:pre [(vector? fg)
           (= (count fg) 3)
           (contains? (set layer-order) layer-id)]}
      (alter (get layers-character-map layer-id)
             (fn [cm] (assoc-in cm [y x :fx-fg-color] fg))))
  (set-fx-bg! [_ layer-id x y bg]
    {:pre [(vector? bg)
           (= (count bg) 3)
           (contains? (set layer-order) layer-id)]}
      (alter (get layers-character-map layer-id)
             (fn [cm] (assoc-in cm [y x :fx-bg-color] bg))))
  (set-fx-char! [_ layer-id x y c]
    {:pre [(contains? (set layer-order) layer-id)]}
    (alter (get layers-character-map layer-id)
           (fn [cm] (assoc-in cm [y x :fx-character] c))))
  (clear-fx! [_ layer-id]
    {:pre [(contains? (set layer-order) layer-id)]}
    (alter (get layers-character-map layer-id)
           (fn [cm]
             (mapv (fn [line]
                     (mapv (fn [c]
                             (assoc c :fx-fg-color nil
                                      :fx-bg-color nil
                                      :fx-character nil))
                           line))
                   cm))))
  (clear-fx! [_]
    (doseq [[_ character-map] layers-character-map]
      (alter character-map
             (fn [cm]
               (mapv (fn [line]
                       (mapv (fn [c]
                               (assoc c :fx-fg-color nil
                                        :fx-bg-color nil
                                        :fx-character nil))
                             line))
                     cm)))))
  (destroy! [_]
    (reset! destroyed true)))


(defn make-terminal
  [layer-order {:keys [title columns rows default-fg-color default-bg-color on-key-fn windows-font else-font font-size antialias icon-paths fx-shader]
    :or {title "Zaffre"
         columns 80
         rows    24
         default fg-color [255 255 255]
         default bg-color [0 0 0]
         on-key-fn        nil
         windows-font     "Courier New"
         else-font        "Monospaced"
         font-size        16
         antialias        true
         icon-paths       nil
         fx-shader        {:name "passthrough.fs"}}}]
    (let [is-windows       (>= (.. System (getProperty "os.name" "") (toLowerCase) (indexOf "win")) 0)
          normal-font      (atom nil)
          font-textures    (atom {})
          antialias        (atom antialias)
          _                (add-watch
                             normal-font
                             :font-watcher
                             (fn [_ _ _ new-font]
                               (log/info "Using font" new-font)
                               (let [font-metrics  ^FontMetrics (.getFontMetrics (Canvas.) new-font)
                                     char-width                 (.charWidth font-metrics \M)
                                     char-height                (.getHeight font-metrics)
                                     screen-width               (* columns char-width)
                                     screen-height              (* rows char-height)
                                     ;; create texture atlas as Buffered texture
                                     {:keys [font-texture-width
                                             font-texture-height
                                             font-texture-image]} (make-glyph-image char-width char-height new-font)]
                                 (log/info "Created font texture. screen-width" screen-width "screen-height" screen-height)
                                 (swap! font-textures assoc
                                                     (font-key new-font)
                                                     {:screen-width screen-width
                                                      :screen-height screen-height
                                                      :character-width char-width
                                                      :character-height char-height
                                                      :font-texture-width font-texture-width
                                                      :font-texture-height font-texture-height
                                                      :font-texture-image font-texture-image}))))
          _                  (reset! normal-font (if is-windows
                                                   (make-font windows-font Font/PLAIN font-size)
                                                   (make-font else-font Font/PLAIN font-size)))
          ;; false if Display is destoyed
          destroyed          (atom false)
          gl-lock            (atom true)
          {:keys [screen-width
                  screen-height
                  character-width
                  character-height
                  font-texture-width
                  font-texture-height
                  font-texture-image]} (get @font-textures (font-key @normal-font))
          _                  (log/info "screen size" screen-width "x" screen-height)
          _                  (init-display title screen-width screen-height icon-paths gl-lock destroyed)

          font-texture       (with-gl-context gl-lock (texture-id font-texture-image))
          _                  (swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture font-texture)))
          ;; create texture atlas
          character-map-cleared (vec (repeat rows (vec (repeat columns (make-terminal-character (char 0) default-fg-color default-bg-color #{})))))
          _                     (log/info "layer-order" layer-order)
          layers-character-map  (zipmap layer-order (repeatedly #(ref character-map-cleared)))
          cursor-xy             (atom nil)

          key-chan         (async/chan)
          on-key-fn        (or on-key-fn
                               (fn default-on-key-fn [k]
                                 (async/put! key-chan k)))

          ;; create width*height texture that gets updated each frame that determines which character to draw in each cell
          _ (log/info "Creating glyph array")
          next-pow-2-columns (next-pow-2 columns)
          next-pow-2-rows    (next-pow-2 rows)
          glyph-texture-width  next-pow-2-columns
          glyph-texture-height next-pow-2-rows
          _ (log/info "creating buffers for glyph/fg/bg textures (" next-pow-2-columns "x" next-pow-2-rows ")")
          ;glyph-array    (ta/unsigned-int8 (repeat (* columns rows) 0))
          glyph-image-data (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4))
          fg-image-data    (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4))
          bg-image-data    (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4))
          glyph-texture    (with-gl-context gl-lock (xy-texture-id next-pow-2-columns next-pow-2-rows glyph-image-data))
          fg-texture       (with-gl-context gl-lock (texture-id next-pow-2-columns next-pow-2-rows fg-image-data))
          bg-texture       (with-gl-context gl-lock (texture-id next-pow-2-columns next-pow-2-rows bg-image-data))
          [fbo-id fbo-texture]
                           (with-gl-context gl-lock (fbo-texture screen-width screen-height))
          ; init shaders
          [^int pgm-id
           ^int fb-pgm-id]      (with-gl-context gl-lock (init-shaders (get fx-shader :name)))
          pos-vertex-attribute            (with-gl-context gl-lock (GL20/glGetAttribLocation pgm-id "aVertexPosition"))
          texture-coords-vertex-attribute (with-gl-context gl-lock (GL20/glGetAttribLocation pgm-id "aTextureCoord"))
          fb-pos-vertex-attribute            (with-gl-context gl-lock (GL20/glGetAttribLocation fb-pgm-id "aVertexPosition"))
          fb-texture-coords-vertex-attribute (with-gl-context gl-lock (GL20/glGetAttribLocation fb-pgm-id "aTextureCoord"))

          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing the terminal.
          {:keys [vertices-vbo-id
                  vertices-count
                  texture-coords-vbo-id
                  vao-id]}          (with-gl-context gl-lock (init-buffers))
          [glyph-tex-dim
           u-MVMatrix
           u-PMatrix
           u-font
           u-glyphs
           u-fg
           u-bg
           font-size
           term-dim
           font-tex-dim]  (with-gl-context gl-lock (mapv #(GL20/glGetUniformLocation pgm-id (str %))
                                                         ["glyphTextureDimensions"
                                                          "uMVMatrix"
                                                          "uPMatrix"
                                                          "uFont"
                                                          "uGlyphs"
                                                          "uFg"
                                                          "uBg"
                                                          "fontSize"
                                                          "termDimensions"
                                                          "fontTextureDimensions"]))
          [u-fb
           u-fb-MVMatrix
           u-fb-PMatrix] (with-gl-context gl-lock (mapv #(GL20/glGetUniformLocation fb-pgm-id (str %))
                                                        ["uFb"
                                                         "uMVMatrix"
                                                         "uPMatrix"]))
          ;; map from uniform name (string) to [value atom, uniform location]
          fx-uniforms    (reduce (fn [uniforms [uniform-name value]]
                                   (assoc uniforms
                                          uniform-name
                                          [(atom value)
                                           (with-gl-context gl-lock
                                             (log/info "getting location of uniform" uniform-name)
                                             (let [location (GL20/glGetUniformLocation fb-pgm-id (str uniform-name))]
                                               #_(assert (not (neg? jocation)) (str "Could not find location for uniform " uniform-name location))
                                               (log/info "got location of uniform" uniform-name location)
                                               location))]))
                                 {}
                                 (get fx-shader :uniforms))
          _ (with-gl-context gl-lock
              (doseq [idx (range (GL20/glGetProgrami fb-pgm-id GL20/GL_ACTIVE_UNIFORMS))]
                (log/info "Found uniform" (GL20/glGetActiveUniform fb-pgm-id idx 100))))
          terminal
          ;; Create and return terminal
          (OpenGlTerminal. columns
                           rows
                           next-pow-2-columns
                           next-pow-2-rows
                           font-textures
                           normal-font
                           fbo-texture
                           antialias
                           character-map-cleared
                           layers-character-map
                           layer-order
                           cursor-xy
                           fx-uniforms
                           {:p-matrix-buffer (ortho-matrix-buffer screen-width screen-height)
                            :mv-matrix-buffer (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
                                                                      [screen-width screen-height 1.0])
                            :character-width character-width
                            :character-height character-height
                            :character->col-row (character->col-row (character-idxs character-width (displayable-characters @normal-font)))
                            :buffers {:vertices-vbo-id vertices-vbo-id
                                      :vertices-count vertices-count
                                      :texture-coords-vbo-id texture-coords-vbo-id
                                      :vao-id vao-id
                                      :fbo-id fbo-id}
                            :textures {:glyph-texture glyph-texture
                                       :font-texture font-texture
                                       :fg-texture fg-texture
                                       :bg-texture bg-texture
                                       :fbo-texture fbo-texture}
                            :attributes {:pos-vertex-attribute pos-vertex-attribute
                                         :texture-coords-vertex-attribute texture-coords-vertex-attribute
                                         :fb-pos-vertex-attribute fb-pos-vertex-attribute
                                         :fb-texture-coords-vertex-attribute fb-texture-coords-vertex-attribute
}
                            :program-id pgm-id
                            :fb-program-id fb-pgm-id
                            :uniforms {:u-MVMatrix u-MVMatrix
                                       :u-PMatrix u-PMatrix
                                       :u-fb-MVMatrix u-fb-MVMatrix
                                       :u-fb-PMatrix u-fb-PMatrix
                                       :u-font u-font
                                       :u-glyphs u-glyphs
                                       :u-fg u-fg
                                       :u-bg u-bg
                                       :font-size font-size
                                       :term-dim term-dim
                                       :font-tex-dim font-tex-dim
                                       :font-texture-width font-texture-width
                                       :font-texture-height font-texture-height
                                       :glyph-tex-dim glyph-tex-dim
                                       :glyph-texture-width glyph-texture-width
                                       :glyph-texture-height glyph-texture-height
                                       :u-fb u-fb}
                            :data {:glyph-image-data glyph-image-data
                                   :fg-image-data fg-image-data
                                   :bg-image-data bg-image-data}}
                           key-chan
                           gl-lock
                           destroyed)]
      ;; Access to terminal will be multi threaded. Release context so that other threads can access it)))
      (Display/releaseContext)
      ;; Start font file change listener thread
      #_(cwc/start-watch [{:path "./fonts"
                         :event-types [:modify]
                         :bootstrap (fn [path] (println "Starting to watch " path))
                         :callback (fn [_ filename]
                                     (println "Reloading font" filename)
                                     (reset! normal-font
                                             (make-font filename Font/PLAIN font-size)))
                         :options {:recursive true}}])
      ;; Poll keyboard in background thread and offer input to key-chan
      ;; If gl-lock is false ie: the window has been closed, put :exit on the key-chan
      (go-loop []
         (with-gl-context gl-lock
           (try
             (loop []
               (when (Keyboard/next)
                 (when (Keyboard/getEventKeyState)
                   (let [character (Keyboard/getEventCharacter)
                         key       (Keyboard/getEventKey)]
                     (convert-key-code character key on-key-fn)))
                 (recur)))
             (catch Exception e
               (log/error "Error getting keyboard input" e))))
         (if @gl-lock
           (do
             (Thread/sleep 5)
             (recur))
           (on-key-fn :exit)))
      terminal))

;; Draw in main thread
(defn draw-in-main
  "Show a terminal and echo input."
  [& _]
  ;; render in background thread
  (let [terminal   (make-terminal [:text]
                                  {:title "Zaffre demo"
                                   :columns 80 :rows 24
                                   :default-fg-color [250 250 250]
                                   :default-bg-color [5 5 8]
                                   :font-size 18
                                   :antialias true
                                   :icon-paths ["images/icon-16x16.png"
                                                "images/icon-32x32.png"
                                                "images/icon-128x128.png"]})
        last-key   (atom nil)
        input-chan (go-loop []
          (reset! last-key (async/<!! (zat/get-key-chan terminal)))
          (log/info "got key" (or (str @last-key) "nil"))
          ;; change font size on s/m/l keypress
          (case @last-key
            \s (zat/apply-font! terminal "Consolas" "Monospaced" 12 true)
            \m (zat/apply-font! terminal "Consolas" "Monospaced" 18 true)
            \l (zat/apply-font! terminal "Consolas" "Monospaced" 24 true)
            nil)
          (recur))]
    ;; get key presses in fg thread
    (loop []
      (let [key-in (or @last-key \?)]
        (dosync
          (zat/clear! terminal)
          (zutil/put-string terminal :text 0 0 "Hello world")
          (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
            (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
          (zutil/put-string terminal :text 12 0 (str key-in))
          (zat/refresh! terminal))
        (if (= key-in :exit)
          (do
            (async/close! input-chan)
            (log/info "Got :exit. Stopping")
            (System/exit 0))
          (recur))))))

;; Handle input in main thread
(defn handle-input-in-main
  "Show a terminal and echo input."
  [& _]
  ;; render in background thread
   (let [colorShift    (atom 0.0001)
         brightness    (atom 0.68)
         contrast      (atom 2.46)
         scanlineDepth (atom 0.94)
         time          (atom 0.0)
         noise         (atom 0.0016)
         terminal   (make-terminal [:text :rainbow]
                                   {:title "Zaffre demo"
                                    :columns 80 :rows 24
                                    :default-fg-color [250 250 250]
                                    :default-bg-color [5 5 8]
                                    :windows-font "Consolas"
                                    :else-font "/home/santos/Downloads/cour.ttf"
                                    :font-size 18
                                    :antialias true
                                    :icon-paths ["images/icon-16x16.png"
                                                 "images/icon-32x32.png"
                                                 "images/icon-128x128.png"]
                                    :fx-shader {:name     "retro.fs"
                                                :uniforms [["time" @time]
                                                           ["noise" @noise]
                                                           ["colorShift" @colorShift]
                                                           ["scanlineDepth" @scanlineDepth]
                                                           ["brightness" @brightness]
                                                           ["contrast" @contrast]]}})
        last-key    (atom nil)
        ;; Every 10ms, set the "Rainbow" text to have a random fg color
        fx-chan     (go-loop []
                      (dosync
                        (doseq [x (range (count "Rainbow"))]
                          (zat/set-fx-fg! terminal :rainbow (inc x) 1 [128 (rand 255) (rand 255)])))
                        (zat/assoc-fx-uniform! terminal "time" (swap! time inc))
                        (zat/refresh! terminal)
                      (Thread/sleep 10)
                      (recur))
        ;; Every 33ms, draw a full frame
        render-chan (go-loop []
                      (dosync
                        (let [key-in (or @last-key \?)]
                          (zat/clear! terminal)
                          (zutil/put-string terminal :text 0 0 "Hello world")
                          (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                            (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                          (zutil/put-string terminal :text 12 0 (str key-in))
                          (zutil/put-string terminal :rainbow 1 1 "Rainbow")
                          (zat/refresh! terminal)))
                          ;; ~30fps
                        (Thread/sleep 33)
                        (recur))]
    ;; get key presses in fg thread
    (loop []
      (let [new-key (async/<!! (zat/get-key-chan terminal))]
        (reset! last-key new-key)
        (log/info "got key" (or (str @last-key) "nil"))
        ;; change font size on s/m/l keypress
        (case new-key
          \s (zat/apply-font! terminal "Consolas" "/home/santos/Downloads/cour.ttf" 12 true)
          \m (zat/apply-font! terminal "Consolas" "/home/santos/Downloads/cour.ttf" 18 true)
          \l (zat/apply-font! terminal "Consolas" "/home/santos/Downloads/cour.ttf" 24 true)
          \0 (zat/assoc-fx-uniform! terminal "brightness" (swap! brightness #(- % 0.02)))
          \1 (zat/assoc-fx-uniform! terminal "brightness" (swap! brightness #(+ % 0.02)))
          \2 (do (swap! contrast #(- % 0.02))
                 (log/info "contrast" @contrast)
                 (zat/assoc-fx-uniform! terminal "contrast" @contrast))
          \3 (zat/assoc-fx-uniform! terminal "contrast" (swap! contrast #(+ % 0.02)))
          \4 (zat/assoc-fx-uniform! terminal "scanlineDepth" (swap! scanlineDepth #(- % 0.02)))
          \5 (zat/assoc-fx-uniform! terminal "scanlineDepth" (swap! scanlineDepth #(+ % 0.02)))
          \6 (zat/assoc-fx-uniform! terminal "colorShift" (swap! colorShift #(- % 0.0001)))
          \7 (zat/assoc-fx-uniform! terminal "colorShift" (swap! colorShift #(+ % 0.0001)))
          \8 (zat/assoc-fx-uniform! terminal "noise" (swap! noise #(- % 0.0001)))
          \9 (zat/assoc-fx-uniform! terminal "noise" (swap! noise #(+ % 0.0001)))
          \p (log/info "brightness" @brightness
                       "contrast" @contrast
                       "scanlineDepth" @scanlineDepth
                       "colorShift" @colorShift
                       "noise" @noise
                       "time" @time)
          \q (zat/destroy! terminal)
          nil)
        (if (= new-key :exit)
          (do
            (async/close! fx-chan)
            (async/close! render-chan)
            (System/exit 0))
          (recur))))))

(defn -main [& _]
  #_(draw-in-main)
  (handle-input-in-main))
