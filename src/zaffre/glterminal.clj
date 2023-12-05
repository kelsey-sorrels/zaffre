; Functions for rendering characters to screen
(ns zaffre.glterminal
  (:require [zaffre.aterminal :as zat]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [zaffre.imageutil :as zimgutil]
            [zaffre.keyboard :as zkeyboard]
            [nio.core :as nio]
            [taoensso.timbre :as log]
            [clojure.core.async :as async :refer [go go-loop]]
            [clojure-watch.core :as cwc])
  (:import
    (java.lang.reflect Field)
    (org.lwjgl BufferUtils)
    (java.nio FloatBuffer ByteBuffer)
    (java.nio.charset Charset)
    (org.lwjgl.opengl GL GLUtil GL11 GL12 GL13 GL14 GL15 GL20 GL30 GL32)
    (org.lwjgl.glfw GLFW GLFWVidMode GLFWErrorCallback GLFWCharCallback GLFWKeyCallback GLFWMouseButtonCallback GLFWCursorPosCallback)
    (org.lwjgl.system Platform)
    (org.joml Matrix4f Vector3f)
    (java.io File FileInputStream FileOutputStream)
    (java.awt.image BufferedImage)
    (zaffre.aterminal ATerminal)
    (zaffre.font CP437Font TTFFont))
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; GLFW won't maintain a strong reference to our callback objects, so we have to
(def key-callback-atom (atom nil))
(def char-callback-atom (atom nil))
(def mouse-button-callback-atom (atom nil))
(def cursor-pos-callback-atom (atom nil))

(defmacro with-gl-context
  "Executes exprs in an implicit do, while holding the monitor of x and aquiring/releasing the OpenGL context.
  Will release the monitor of x in all circumstances."
  [x w c & body]
  `(let [lockee# ~x
         window# ~w
         capabilities# ~c]
     (try
       (monitor-enter lockee#)
       (when @lockee#
         (GLFW/glfwMakeContextCurrent window#)
         (GL/setCapabilities capabilities#)
         ~@body)
       (finally
         (when @lockee#
           (GLFW/glfwMakeContextCurrent 0))
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

(defn font-key [font] font)

(defn- get-fields [#^Class static-class]
  (.getFields static-class))

(defn- gl-enum-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (zero? enum-value)
    "NONE"
    (.getName #^Field (some
                       #(when (= enum-value (.get #^Field % nil)) %)
                       (mapcat get-fields [GL11 GL12 GL13 GL15 GL20 GL30])))))

(defn- except-gl-errors
  [msg]
  nil
  (let [error (GL11/glGetError)
        error-string (str "OpenGL Error(" error "):"
                          (gl-enum-name error) ": " msg " - "
                          (GLUtil/getErrorString error))]
    (if (not (zero? error))
      (throw (Exception. error-string)))))

(defn- texture-id-2d
  ([^BufferedImage buffered-image]
  (let [width          (.getWidth buffered-image)
        height         (.getHeight buffered-image)
        texture-id     (GL11/glGenTextures)
        texture-buffer ^ByteBuffer (zimgutil/buffered-image-byte-buffer buffered-image)]
     (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
     (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
     (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
     (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
     (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE texture-buffer)
     (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
     (except-gl-errors "end of texture-id-2d")
     texture-id)))

(defn- texture-id
  ([^BufferedImage buffered-image]
  (let [width  (.getWidth buffered-image)
        height (.getHeight buffered-image)]
    (texture-id width height 1 (zimgutil/buffered-image-byte-buffer buffered-image))))
  ([width height layers]
   (texture-id width height layers (BufferUtils/createByteBuffer (* width height 4 layers))))
  ([^long width ^long height ^long layers ^ByteBuffer texture-buffer]
   (let [texture-id (GL11/glGenTextures)]
     (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY texture-id)
     (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
     (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
     (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
     (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL11/GL_RGBA width height layers 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE texture-buffer)
     (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY 0)
     (except-gl-errors "end of texture-id")
     texture-id)))

(defn- xy-texture-id [^long width ^long height ^long layers ^ByteBuffer texture-buffer]
  (let [texture-id (GL11/glGenTextures)]
    (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY texture-id)
    (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
    (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL30/GL_RGBA8UI width height layers 0 GL30/GL_RGBA_INTEGER GL11/GL_INT texture-buffer)
    (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY 0)
    (except-gl-errors "end of xy-texture-id")
    texture-id))

(defn- except-framebuffer-status [fbo-id]
  (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER fbo-id)
  (let [fb-status (GL30/glCheckFramebufferStatus GL30/GL_FRAMEBUFFER)]
    (log/info fb-status)
    (except-gl-errors "Framebuffer not complete")
    (when (not= fb-status GL30/GL_FRAMEBUFFER_COMPLETE)
      (throw (Exception. (case fb-status
                           GL30/GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT "at least one attachment point with a renderbuffer or textur attached has its attached object no longer in existence or has an attached image with a width or height of zero"
                           GL30/GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT "No images are attached to the framebuffer"
                           GL30/GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER "GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER"
                           GL30/GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE "GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE"
                           GL30/GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER "GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER"
                           GL30/GL_FRAMEBUFFER_UNSUPPORTED "The combination of internal formats of the attached images violates an implementation-dependent set of restrictions"
                           0 "Got 0 for framebuffer status. An error occurred"))))))
  

(defn- fbo-texture [^long width ^long height]
  (let [fbo-id     (GL30/glGenFramebuffers)
        texture-id (GL11/glGenTextures)
        ;; type nil as ByteBuffer to avoid reflection on glTexImage2D
        ^ByteBuffer bbnil nil]
    (log/info "Creating framebuffer" width "x" height)
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER fbo-id)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bbnil)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
    (except-gl-errors "end of fbo-texture")

    (GL32/glFramebufferTexture GL30/GL_FRAMEBUFFER GL30/GL_COLOR_ATTACHMENT0 texture-id 0)
    (GL11/glDrawBuffer GL30/GL_COLOR_ATTACHMENT0)
    (except-framebuffer-status fbo-id)
    (except-gl-errors "end of fbo")
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
    [fbo-id texture-id]))
        

(defn platform []
  (condp = (Platform/get)
    Platform/LINUX :linux
    Platform/MACOSX :macosx
    Platform/WINDOWS :windows))

;; Extract native libs and setup system properties
(defn init-natives []
  (when (.exists (File. "natives"))
  ;(System/setProperty "java.library.path", (.getAbsolutePath (File. "natives")))
  (condp = [(platform) (.endsWith (System/getProperty "os.arch") "64")]
    [:linux false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/linux/x86")))
    [:linux true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/linux/x86_64")))
    [:macosx false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/macosx/x86")))
    [:macosx true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/macosx/x86_64")))
    [:windows false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/windows/x86")))
    [:windows true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/windows/x86_64"))))))

(defn- init-display [title screen-width screen-height icon-paths gl-lock destroyed]
  (let [icon-array         (when icon-paths
                             (condp = (Platform/get)

                               Platform/LINUX   (into-array ByteBuffer [(zimgutil/png-bytes (get icon-paths 1))])
                               Platform/MACOSX  (into-array ByteBuffer [(zimgutil/png-bytes (get icon-paths 2))])
                               Platform/WINDOWS (into-array ByteBuffer [(zimgutil/png-bytes (get icon-paths 0))
                                                                        (zimgutil/png-bytes (get icon-paths 1))])))]
     ;; init-natives must be called before the Display is created
     (init-natives)
     ;;(GLFW/glfwSetErrorCallback (GLFWErrorCallback/createPrint System/out))
     (GLFW/glfwSetErrorCallback (proxy [GLFWErrorCallback] []
                                           (invoke [error description] (log/error "GLFW error:" error description))))
     ;(GLUtil/setupDebugMessageCallback System/out)
     (when-not (= (GLFW/glfwInit) GLFW/GLFW_TRUE)
       (assert "Unable to initialize GLFW"))
     (GLFW/glfwDefaultWindowHints)
     (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
     (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_FALSE)
     
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
     (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 2)
     (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
     (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GLFW/GLFW_TRUE)
     (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_DEBUG_CONTEXT GLFW/GLFW_TRUE)
     (if-let [window (GLFW/glfwCreateWindow (int screen-width) (int screen-height) (str title) 0 0)]
       (do
         ; TODO: set window icons
         #_(when icon-array
           (log/info "Setting icons")
           ;(Display/setIcon icon-array)
           (Thread/sleep 100))
         (log/info "byte-buffer" icon-array)
         (let [vidmode      (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))]
           (GLFW/glfwSetWindowPos
             window
             (/ (- (.width vidmode) screen-width) 2)
             (/ (- (.height vidmode) screen-height) 2))
           (GLFW/glfwMakeContextCurrent window)
           (GLFW/glfwSwapInterval 1)
           (GLFW/glfwShowWindow window)
           (let [capabilities (GL/createCapabilities)
                 width-buffer (BufferUtils/createIntBuffer 1)
                 height-buffer (BufferUtils/createIntBuffer 1)]
             (GLFW/glfwGetFramebufferSize window width-buffer height-buffer)
             (let [framebuffer-width (.get width-buffer)
                   framebuffer-height (.get height-buffer)]
               (GL11/glViewport 0 0 framebuffer-width framebuffer-height)
               (GLFW/glfwMakeContextCurrent 0)
               ;; Signal to parent that display has been created
               [window
                capabilities
                framebuffer-width
                framebuffer-height]))))
     (throw (RuntimeException. "Failed to create the GLFW window")))))

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
    ; TODO: use simpler method to construct matrix
    #_(let [ortho-matrix (doto (Matrix4f.)
                             (.ortho2D 0 viewport-width 0 viewport-height))
          matrix-buffer matrix-buffer]
          (.clear matrix-buffer)
          (.get ortho-matrix matrix-buffer))
    (let [ortho-matrix (doto (Matrix4f.) (.identity))
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
      (.clear matrix-buffer)
      (.get ortho-matrix matrix-buffer)
      matrix-buffer)))

(defn position-matrix-buffer
  ([v s]
   (position-matrix-buffer v s (BufferUtils/createFloatBuffer 16)))
  ([v s ^FloatBuffer matrix-buffer]
    (let [matrix (doto (Matrix4f.)
                       (.identity))]
      (.translate matrix (Vector3f. (get v 0) (get v 1) (get v 2)))
      (.scale matrix (Vector3f. (get s 0) (get s 1) (get s 2)))
      (.clear matrix-buffer)
      (.get matrix matrix-buffer)
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

(defn- fill-glyph-fg-bg-buffers [layer-id->character-map character->col-row character->transparent texture-columns texture-rows rows layer-size cursor-xy glyph-image-data fg-image-data bg-image-data]
  (let [^long texture-columns texture-columns
        ^long texture-rows texture-rows
        ^long rows rows
        ^long layer-size layer-size
        ^ByteBuffer glyph-image-data glyph-image-data
        ^ByteBuffer fg-image-data fg-image-data
        ^ByteBuffer bg-image-data bg-image-data]
    (.clear glyph-image-data)
    (.clear fg-image-data)
    (.clear bg-image-data)
    (loop-with-index layer [[layer-id character-map] layer-id->character-map]
      ;;(doseq [[layer-id character-map] layers-character-map]
      ;;  (doseq [[row line] (map-indexed vector (reverse @character-map))
      ;;          [col c]    (map-indexed vector line)]
          ;;(log/info "row" row "col" col "c" c)
      ;(log/info layer-id)
      ;; Set buffer positions to beginning
      (loop-with-index row [line (reverse @character-map)]
        (loop-with-index col [c line]
          (let [chr        (or (get c :fx-character) (get c :character))
                highlight  (= @cursor-xy [col (- rows row 1)])
                [fg-r fg-g fg-b] (if highlight
                                   (or (get c :fx-bg-color)  (get c :bg-color))
                                   (or (get c :fx-fg-color)  (get c :fg-color)))
                [bg-r bg-g bg-b] (if highlight
                                   (or (get c :fx-fg-color)  (get c :fg-color))
                                   (or (get c :fx-bg-color)  (get c :bg-color)))
                ;s         (str (get c :character))
                style     (get c :style)
                i         (+ (* 4 (+ (* texture-columns row) col)) (* layer-size layer))
                [x y]     (get character->col-row chr)
                transparent (get character->transparent chr)]
            ;(log/info "Drawing " layer-id "at col row" col row "character from atlas col row" x y c "(index=" i ") transparent" transparent )
            (when (zero? col)
              ;(log/info "glyph texture" glyph-texture-width glyph-texture-height)
              ;(log/info "resetting position to start of line" layer row col i)
              (.position glyph-image-data i)
              (.position fg-image-data i)
              (.position bg-image-data i))
            (if (not= chr (char 0))
              (do
                (assert (or (not (nil? x)) (not (nil? y)))
                        (format "X/Y nil - glyph not found for character %s %s"
                          (or (str chr) "nil")
                          (or (cond
                                (nil? chr) "nil"
                                (char? chr) (format "%x" (int chr))
                                :else (str chr)))))
                (.put glyph-image-data (unchecked-byte x))
                (.put glyph-image-data (unchecked-byte y))
                ;; TODO fill with appropriate type
                (.put glyph-image-data (unchecked-byte (cond
                                                         transparent 2
                                                         :else 1)))
                (.put glyph-image-data (unchecked-byte 0))
                (.put fg-image-data    (unchecked-byte fg-r))
                (.put fg-image-data    (unchecked-byte fg-g))
                (.put fg-image-data    (unchecked-byte fg-b))
                (.put fg-image-data    (unchecked-byte 0))
                (.put bg-image-data    (unchecked-byte bg-r))
                (.put bg-image-data    (unchecked-byte bg-g))
                (.put bg-image-data    (unchecked-byte bg-b))
                (.put bg-image-data    (unchecked-byte 0)))
              ;; space ie empty, skip forward
              (do
                (.put glyph-image-data (byte-array 4))
                (.put fg-image-data (byte-array 4))
                (.put bg-image-data (byte-array 4)))))))
        #_(log/info "At pos" (.position glyph-image-data))
        #_(log/info "Setting layer" layer "new pos" (* texture-columns texture-height 4 (inc layer)))
        (.position glyph-image-data (* texture-columns texture-rows 4 (inc layer)))
        (.position fg-image-data    (* texture-columns texture-rows 4 (inc layer)))
        (.position bg-image-data    (* texture-columns texture-rows 4 (inc layer))))
      (.flip glyph-image-data)
      (.flip fg-image-data)
      (.flip bg-image-data)))

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

(defrecord OpenGlTerminal [;^int columns
                           ;^int rows
                           ;^int texture-columns
                           ;^int texture-rows
                           ;font-textures
                           ;normal-font
                           fbo-texture
                           fullscreen
                           antialias
                           layer-character-map-cleared
                           layer-character-map
                           group-map
                           group->font-texture
                           layer-id->group
                           cursor-xy
                           fx-uniforms
                           gl
                           term-chan
                           term-pub
                           gl-lock
                           destroyed
                           window
                           capabilities]
  zat/ATerminal
  (get-size [_]
    [0 0])
  (alter-group-pos! [_ group-id f]
    (alter (get group-map group-id) (fn [group] (update group :pos (fn [pos] (mapv int (f pos)))))))
  ;; characters is a list of {:c \character :x col :y row :fg [r g b] :bg [r g b]}
  (put-chars! [_ layer-id characters]
    {:pre [(get layer-id->group layer-id)
           (get layer-character-map layer-id)]}
    #_(log/info "characters" (str characters))
    (let [columns (-> layer-id layer-id->group deref :columns)
          rows    (-> layer-id layer-id->group deref :rows)]
      ;(log/info "writing to layer" layer-id "dim:" columns "x" rows)
      (alter (get layer-character-map layer-id)
             (fn [cm]
               (reduce (fn [cm [row row-characters]]
                         (if (< -1 row rows)
                           (assoc cm
                             (int row)
                             (persistent!
                               (reduce
                                 (fn [line c]
                                   (if (< -1 (get c :x) columns)
                                     (let [fg        (get c :fg)
                                           bg        (get c :bg)
                                           fg-color  fg
                                           bg-color  bg
                                           character (make-terminal-character (get c :c) fg-color bg-color #{})]
                                       (assoc! line (int (get c :x)) character))
                                     line))
                                 (transient (get cm (int row)))
                                 row-characters)))
                           cm))
                       cm
                       (group-by :y characters))))
      #_(log/info "character-map" (str @character-map))))
  (set-fg! [_ layer-id x y fg]
    {:pre [(vector? fg)
           (= (count fg) 3)]}
      (alter (get layer-character-map layer-id)
             (fn [cm] (assoc-in cm [y x :fg-color] fg))))
  (set-bg! [_ layer-id x y bg]
    {:pre [(vector? bg)
           (= (count bg) 3)]}
      (alter (get layer-character-map)
             (fn [cm] (assoc-in cm [y x :bg-color] bg))))
  (assoc-fx-uniform! [_ k v]
    (-> fx-uniforms (get k) first (reset! v)))
  (pub [_]
    term-pub)
  (apply-font! [_ group-id font-fn]
    (with-gl-context gl-lock window capabilities
      (alter (get group->font-texture group-id)
             (fn [font-texture]
               (let [font (font-fn (platform))
                     gg   (zfont/glyph-graphics font)
                     font-texture (texture-id-2d (get gg :font-texture-image))]
                 (log/info "loaded font-texture for" group-id font-texture)
                 (assoc gg :font-texture font-texture))))))
          ; TODO: uncomment when LWJGL version is newer
          ;;(when-not @fullscreen
          ;;  (GLFW/glfwSetWindowMonitor window
          ;;                             (GLFW/glfwGetWindowMonitor window)
          ;;                             0
          ;;                             0
          ;;                             (* columns character-width)
          ;;                             (* rows character-height)
          ;;                             60))
          ;;;; resize FBO
          ;;(GL11/glBindTexture GL11/GL_TEXTURE_2D fbo-texture)
          ;;(GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB (int framebuffer-width) (int framebuffer-height) 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE bbnil)
          ;;(swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture (texture-id-2d font-texture-image))))
          ;;(catch Throwable t
          ;;  (log/error "Error changing font" t))))))
  (set-cursor! [_ x y]
    (reset! cursor-xy [x y]))
  (refresh! [_]
    (with-gl-context gl-lock window capabilities
      (let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id vao-id fbo-id]} :buffers
             {:keys [glyph-textures fg-textures bg-textures fbo-texture]} :textures
             program-id :program-id
             fb-program-id :fb-program-id
             {:keys [u-MVMatrix u-PMatrix u-fb-MVMatrix u-fb-PMatrix u-font u-glyphs u-fg u-bg u-num-layers font-size term-dim font-tex-dim
                     font-texture-width font-texture-height glyph-tex-dim
                     u-fb framebuffer-width framebuffer-height]} :uniforms
             ;; TODO move typehints because these are now lists of ByteBuffer elements
             {:keys [glyph-image-data
                     fg-image-data
                     bg-image-data]} :data
             :keys [p-matrix-buffer mv-matrix-buffer]} gl
            ;glyph-image-data glyph-image-data
            ;fg-image-data fg-image-data
            ;bg-image-data bg-image-data
            ;{:keys [character-width
            ;        character-height
            ;        character->col-row
            ;        character->transparent
            ;        font-texture-width
            ;        font-texture-height
            ;        font-texture]} (get @font-textures (font-key @normal-font))
            [display-width display-height] (let [mode (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))]
                                             [(.width mode) (.height mode)])]
        ;(log/info "drawing with" (mapv vec [glyph-textures fg-textures bg-textures glyph-image-data fg-image-data bg-image-data]))
        ;; Setup render to FBO
        (try
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER, fbo-id)
          (GL11/glEnable GL11/GL_BLEND)
          (GL11/glDisable GL11/GL_DEPTH_TEST)
          (GL14/glBlendEquation GL14/GL_FUNC_ADD)
          (GL11/glBlendFunc GL11/GL_ONE GL11/GL_ONE_MINUS_SRC_ALPHA)
          (GL11/glViewport 0 0 framebuffer-width framebuffer-height)
          (except-gl-errors (str "glViewport " framebuffer-width framebuffer-height))
          (GL11/glClearColor 0.0 0.0 0.0 0.0)
          (except-gl-errors (str "glClearColor  " 0.0 0.0 1.0 1.0))
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (except-gl-errors (str "glClear  " (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT)))
          (GL20/glUseProgram program-id)
          (GL20/glUniformMatrix4fv u-PMatrix false (ortho-matrix-buffer framebuffer-width framebuffer-height p-matrix-buffer))
          (except-gl-errors (str "u-PMatrix - glUniformMatrix4  " u-PMatrix))
          ; Bind VAO
          (GL30/glBindVertexArray vao-id)
          (except-gl-errors (str "vao bind - glBindVertexArray " vao-id))
          ; Setup vertex buffer
          (except-gl-errors (str "vbo bind - glBindBuffer " vertices-vbo-id))
          (GL20/glEnableVertexAttribArray 0);pos-vertex-attribute
          (except-gl-errors "vbo bind - glEnableVertexAttribArray")
          (except-gl-errors "vbo bind - glVertexAttribPointer")
          ; Setup uv buffer
          (GL20/glEnableVertexAttribArray 1);texture-coords-vertex-attribute
          (except-gl-errors "texture coords bind")
          (catch Error e
           (log/error "OpenGL error:" e)))
        ;; Render each layer to FBO
        (doseq [[{:keys [group-id columns rows layers] [x-pos y-pos] :pos}
                 glyph-texture
                 fg-texture
                 bg-texture
                 glyph-image-data
                 fg-image-data
                 bg-image-data] (map
                                  vector
                                  (dosync (map (fn [[group-id group-ref]] (assoc @group-ref :group-id group-id)) group-map))
                                  glyph-textures
                                  fg-textures
                                  bg-textures
                                  glyph-image-data
                                  fg-image-data
                                  bg-image-data)
                :let [{:keys [character-width
                              character-height
                              character->col-row
                              character->transparent
                              font-texture-width
                              font-texture-height
                              font-texture]} (-> group->font-texture group-id deref)
                      ^ByteBuffer glyph-image-data glyph-image-data
                      ^ByteBuffer fg-image-data fg-image-data
                      ^ByteBuffer bg-image-data bg-image-data
                      texture-columns (int (zutil/next-pow-2 columns))
                      texture-rows    (int (zutil/next-pow-2 rows))
                      layer-count     (count layers)
                      layer-size      (* 4 texture-columns texture-rows)]]
          (assert (not (nil? font-texture-width)) "font-texture-width nil")
          (assert (not (nil? font-texture-height)) "font-texture-height")
          (assert (not (nil? font-texture)) "font-texture nil")
          ;(log/info "rendering layers")
          ;(log/info character->transparent)
          (fill-glyph-fg-bg-buffers (select-keys layer-character-map layers) character->col-row character->transparent texture-columns texture-rows rows layer-size cursor-xy glyph-image-data fg-image-data bg-image-data)
          #_(doseq [layer (partition layer-size (vec (nio/buffer-seq glyph-image-data)))]
            (log/info "layer")
            (doseq [line (partition (* 4 texture-columns) layer)]
              (log/info (vec line))))
          (try
            (GL20/glUniformMatrix4fv
              u-MVMatrix
              false
              (position-matrix-buffer
                [(+ x-pos (- (/ framebuffer-width 2))) (+ y-pos (- (/ framebuffer-height 2))) -1.0 0.0]
                [(* character-width columns) (* character-height rows) 1.0]
                mv-matrix-buffer))
            (except-gl-errors (str "u-MVMatrix - glUniformMatrix4  " u-MVMatrix))
            ; Setup uniforms for glyph, fg, bg, textures
            (GL20/glUniform1i u-font 0)
            (GL20/glUniform1i u-glyphs 1)
            (GL20/glUniform1i u-fg 2)
            (GL20/glUniform1i u-bg 3)
            (GL20/glUniform1i u-num-layers layer-count)
            (except-gl-errors "uniformli bind")
            (log/info "drawing" group-id)
            (log/info "font-size" character-width character-height)
            (log/info "term-dim" columns rows)
            (log/info "font-tex-dim" font-texture-width font-texture-height)
            (log/info "glyph-tex-dim" texture-columns texture-rows)
            (GL20/glUniform2f font-size character-width character-height)
            (GL20/glUniform2f term-dim columns rows)
            (GL20/glUniform2f font-tex-dim font-texture-width font-texture-height)
            (GL20/glUniform2f glyph-tex-dim texture-columns texture-rows)
            (except-gl-errors "uniform2f bind")
            (except-gl-errors "gl(en/dis)able")
            ; Bind font texture
            (log/info "binding font texture" font-texture)
            (GL13/glActiveTexture GL13/GL_TEXTURE0)
            (except-gl-errors "font texture glActiveTexture")
            (GL11/glBindTexture GL11/GL_TEXTURE_2D font-texture)
            (except-gl-errors "font texture glBindTexture")
            ; Send updated glyph texture to gl
            (GL13/glActiveTexture GL13/GL_TEXTURE1)
            (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY glyph-texture)
            (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL30/GL_RGBA8UI texture-columns texture-rows layer-count 0 GL30/GL_RGBA_INTEGER GL11/GL_UNSIGNED_BYTE glyph-image-data)
            (except-gl-errors "glyph texture data")
            ; Send updated fg texture to gl
            (GL13/glActiveTexture GL13/GL_TEXTURE2)
            (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY fg-texture)
            (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL11/GL_RGBA texture-columns texture-rows layer-count 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE fg-image-data)
            (except-gl-errors "fg color texture data")
            ; Send updated bg texture to gl
            (GL13/glActiveTexture GL13/GL_TEXTURE3)
            (except-gl-errors "bg color glActiveTexture")
            (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY bg-texture)
            (except-gl-errors "bg color glBindTexture")
            (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL11/GL_RGBA texture-columns texture-rows layer-count 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bg-image-data)
            (except-gl-errors "bg color glTexImage2D")
            (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
            (except-gl-errors "bg color glDrawArrays")
            (catch Error e
             (log/error "OpenGL error:" e))))
        (try 
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)

          ;; Draw fbo to screen
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
          (GL11/glViewport 0 0 framebuffer-width framebuffer-height)
          (GL11/glClearColor 0.0 0.0 0.0 0.0)
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (GL20/glUseProgram fb-program-id)
          (GL20/glUniformMatrix4fv u-fb-PMatrix false (ortho-matrix-buffer framebuffer-width framebuffer-height p-matrix-buffer))
          (except-gl-errors (str "u-fb-PMatrix - glUniformMatrix4  " u-fb-PMatrix))
          (GL20/glUniformMatrix4fv
            u-fb-MVMatrix
            false
            (position-matrix-buffer
              [(- (/ framebuffer-width 2)) (- (/ framebuffer-height 2)) -1.0 0.0]
              [framebuffer-width framebuffer-height 1.0]
              mv-matrix-buffer))
          (except-gl-errors (str "u-fb-MVMatrix - glUniformMatrix4  " u-fb-MVMatrix))
          (GL20/glEnableVertexAttribArray 0);pos-vertex-attribute
          (except-gl-errors "vbo bind - glEnableVertexAttribArray")
          ;;; Setup uv buffer
          (GL20/glEnableVertexAttribArray 1);texture-coords-vertex-attribute
          (except-gl-errors "texture coords bind")
          ;;; Setup font texture
          (GL13/glActiveTexture GL13/GL_TEXTURE0)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D fbo-texture)
          (GL20/glUniform1i u-fb 0)
          ;;; Set fx uniforms
          ;(log/info "fx-uniforms" (vec fx-uniforms))
          (doseq [[uniform-name [value-atom location]] fx-uniforms]
            (when-not (neg? location)
              (GL20/glUniform1f location (-> value-atom deref float))))
          ;; Draw fx shaded terminal
          (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
          ;;;; clean up
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)
          (GL30/glBindVertexArray 0)
          (except-gl-errors "end of refresh")
          (GLFW/glfwSwapBuffers window)
          (except-gl-errors "end of update")
          (catch Error e
            (log/error "OpenGL error:" e))))))
  (clear! [_]
    (doseq [[id character-map] layer-character-map]
      (ref-set character-map (get layer-character-map-cleared id))))
  (clear! [_ layer-id]
    {:pre [(get layer-id->group layer-id)]}
    (ref-set (get layer-character-map layer-id) (get layer-character-map-cleared layer-id)))
  (fullscreen! [_ v]
    (with-gl-context gl-lock window capabilities
      #_(if (false? v)
        (let [{:keys [framebuffer-width framebuffer-height]} (get @font-textures (font-key @normal-font))]
          (reset! fullscreen false)
          ; TODO: Uncomment when LGJWL version is upgraded
          #_(GLFW/glfwSetWindowMonitor window
                                     (GLFW/glfwGetWindowMonitor window)
                                     0
                                     0
                                     (* columns character-width)
                                     (* rows character-height)
                                     60)
        (let [[width height mode] v]
          (reset! fullscreen true)
          ; TODO: Uncomment when LGJWL version is upgraded
          #_(GLFW/glfwSetWindowMonitor window
                                     (GLFW/glfwGetWindowMonitor window)
                                     0
                                     0
                                     (.width mode)
                                     (.height mode)
                                     60))))))
  (fullscreen-sizes [_]
    (with-gl-context gl-lock window capabilities
      (let [desktop-mode     (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))
            modes            (GLFW/glfwGetVideoModes (GLFW/glfwGetPrimaryMonitor))
            compatible-modes (filter (fn [^GLFWVidMode mode]
                                       (= (.refreshRate mode) (.refreshRate desktop-mode)))
                                     (map (fn [i] (.get modes (int i))) (range (.capacity modes))))]
        (mapv (fn [^GLFWVidMode mode]
                [(.width mode)
                 (.height mode)
                 mode])
              compatible-modes))))
  (set-fx-fg! [_ layer-id x y fg]
    {:pre [(vector? fg)
           (= (count fg) 3)
           (get layer-id->group layer-id)]}
      (alter (get layer-character-map layer-id)
             (fn [cm] (assoc-in cm [y x :fx-fg-color] fg))))
  (set-fx-bg! [_ layer-id x y bg]
    {:pre [(vector? bg)
           (= (count bg) 3)
           (get layer-id->group layer-id)]}
      (alter (get layer-character-map layer-id)
             (fn [cm] (assoc-in cm [y x :fx-bg-color] bg))))
  (set-fx-char! [_ layer-id x y c]
    {:pre [(get layer-id->group layer-id)]}
    (alter (get layer-character-map layer-id)
           (fn [cm] (assoc-in cm [y x :fx-character] c))))
  (clear-fx! [_ layer-id]
    {:pre [(get layer-id->group layer-id)]}
    (alter (get layer-character-map layer-id)
           (fn [cm]
             (mapv (fn [line]
                     (mapv (fn [c]
                             (assoc c :fx-fg-color nil
                                      :fx-bg-color nil
                                      :fx-character nil))
                           line))
                   cm))))
  (clear-fx! [_]
    (doseq [[_ character-map] layer-character-map]
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
    (reset! destroyed true)
    (async/put! term-chan :close)))


(defn make-terminal
  [group-map {:keys [title screen-width screen-height default-fg-color default-bg-color on-key-fn windows-font else-font font-size fullscreen antialias icon-paths fx-shader]
    :or {title "Zaffre"
         default-fg-color [255 255 255]
         default-bg-color [0 0 0]
         on-key-fn        nil
         fullscreen       false
         antialias        true
         icon-paths       nil
         fx-shader        {:name "passthrough.fs"}}}
   f]
    (let [group-map        (into {}
                             (map (fn [[k v]]
                                    [k (ref v)])
                                  group-map))
          layer-id->group  (->> group-map
                              (mapcat (fn [[id layer-group]]
                                        (map (fn [layer-id]
                                               [layer-id layer-group])
                                             (get @layer-group :layers))))
                              (into {}))
          _                     (log/info "group-map" group-map)
          _                     (log/info "layer-id->group" layer-id->group)
          font-textures    (atom {})
          fullscreen       (atom fullscreen)
          antialias        (atom antialias)
          ;; Last [col row] of mouse movement
          mouse-col-row      (atom nil)
          mousedown-col-row (atom nil)
          ;; false if Display is destoyed
          destroyed          (atom false)
          gl-lock            (atom true)
          latch              (java.util.concurrent.CountDownLatch. 1)
          [window
           capabilities
           framebuffer-width
           framebuffer-height]
                             (init-display title screen-width screen-height icon-paths gl-lock destroyed)
          _                  (log/info "window" window "capabilities" capabilities)
          _                  (log/info "framebuffer size" framebuffer-width "x" framebuffer-height)

          group->font-texture
                           (with-gl-context gl-lock window capabilities
                             (into {}
                               (mapv (fn [[k v]]
                                       [k (ref (let [font ((get @v :font) (platform))
                                                     gg   (zfont/glyph-graphics font)
                                                     font-texture (texture-id-2d (get gg :font-texture-image))]
                                                 (log/info "loaded font-texture for" k font-texture (select-keys gg [:font-texture-width :font-texture-height :character-width :character-height]))
                                                 (assoc gg :font-texture font-texture)))])
                                     group-map)))
          ;; create empty character maps
          layer-character-map-cleared (into {}
                                        (for [[id group-ref] layer-id->group
                                              :let [{:keys [columns rows]} @group-ref]]
                                          [id (vec (repeat rows (vec (repeat columns (make-terminal-character
                                                                                       (char 0)
                                                                                       default-fg-color
                                                                                       default-bg-color
                                                                                       #{})))))]))
          layer-character-map  (into {}
                                     (for [id (keys layer-id->group)]
                                       [id (ref (get layer-character-map-cleared id))]))
          cursor-xy             (atom nil)

          term-chan       (async/chan (async/buffer 100))
          ;; Turn term-chan into a pub(lishable) channel
          term-pub        (async/pub term-chan (fn [v]
                                        (cond
                                          (char? v)    :keypress
                                          (keyword? v) v
                                          :else        (get v :type))))
                                        
          on-key-fn        (or on-key-fn
                               (fn alt-on-key-fn [k]
                                 (async/put! term-chan k)))

          ;; create width*height texture that gets updated each frame that determines which character to draw in each cell
          _ (log/info "Creating glyph array")
          glyph-image-data (for [[_ layer-group] group-map]
                             (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                   np2-rows    (zutil/next-pow-2 (get @layer-group :rows))]
                               (log/info "creating buffers for glyph/fg/bg textures" np2-columns "x" np2-rows)
                               (BufferUtils/createByteBuffer (* np2-columns np2-rows 4 (count (get @layer-group :layers))))))
          fg-image-data    (for [[_ layer-group] group-map]
                             (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                   np2-rows    (zutil/next-pow-2 (get @layer-group :rows))]
                               (BufferUtils/createByteBuffer (* np2-columns np2-rows 4 (count (get @layer-group :layers))))))
          bg-image-data    (for [[_ layer-group] group-map]
                             (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                   np2-rows    (zutil/next-pow-2 (get @layer-group :rows))]
                               (BufferUtils/createByteBuffer (* np2-columns np2-rows 4 (count (get @layer-group :layers))))))
          glyph-textures   (with-gl-context gl-lock window capabilities
                             (mapv (fn [layer-group glyph-image-data]
                                     (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                           np2-rows    (zutil/next-pow-2 (get @layer-group :rows))]
                                       (xy-texture-id np2-columns np2-rows (count (get @layer-group :layers)) glyph-image-data)))
                                   (vals group-map)
                                   glyph-image-data))
          fg-textures      (with-gl-context gl-lock window capabilities
                             (mapv (fn [layer-group fg-image-data]
                                     (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                           np2-rows    (zutil/next-pow-2 (get @layer-group :rows))]
                                       (texture-id np2-columns np2-rows (count (get @layer-group :layers)) fg-image-data)))
                                   (vals group-map)
                                   fg-image-data))
          bg-textures      (with-gl-context gl-lock window capabilities
                             (mapv (fn [layer-group bg-image-data]
                                     (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                           np2-rows    (zutil/next-pow-2 (get @layer-group :rows))]
                                       (texture-id np2-columns np2-rows (count (get @layer-group :layers)) bg-image-data)))
                                   (vals group-map)
                                   bg-image-data))
          [fbo-id fbo-texture]
                           (with-gl-context gl-lock window capabilities (fbo-texture framebuffer-width framebuffer-height))
          ; init shaders
          [^int pgm-id
           ^int fb-pgm-id]      (with-gl-context gl-lock window capabilities (init-shaders (get fx-shader :name)))
          pos-vertex-attribute            (with-gl-context gl-lock window capabilities (GL20/glGetAttribLocation pgm-id "aVertexPosition"))
          texture-coords-vertex-attribute (with-gl-context gl-lock window capabilities (GL20/glGetAttribLocation pgm-id "aTextureCoord"))
          fb-pos-vertex-attribute            (with-gl-context gl-lock window capabilities (GL20/glGetAttribLocation fb-pgm-id "aVertexPosition"))
          fb-texture-coords-vertex-attribute (with-gl-context gl-lock window capabilities (GL20/glGetAttribLocation fb-pgm-id "aTextureCoord"))

          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing the terminal.
          {:keys [vertices-vbo-id
                  vertices-count
                  texture-coords-vbo-id
                  vao-id]}          (with-gl-context gl-lock window capabilities (init-buffers))
          [glyph-tex-dim
           u-MVMatrix
           u-PMatrix
           u-font
           u-glyphs
           u-fg
           u-bg
           u-num-layers
           font-size
           term-dim
           font-tex-dim]  (with-gl-context gl-lock window capabilities (mapv #(GL20/glGetUniformLocation pgm-id (str %))
                                                         ["glyphTextureDimensions"
                                                          "uMVMatrix"
                                                          "uPMatrix"
                                                          "uFont"
                                                          "uGlyphs"
                                                          "uFg"
                                                          "uBg"
                                                          "numLayers"
                                                          "fontSize"
                                                          "termDimensions"
                                                          "fontTextureDimensions"]))
          [u-fb
           u-fb-MVMatrix
           u-fb-PMatrix] (with-gl-context gl-lock window capabilities (mapv #(GL20/glGetUniformLocation fb-pgm-id (str %))
                                                        ["uFb"
                                                         "uMVMatrix"
                                                         "uPMatrix"]))
          ;; map from uniform name (string) to [value atom, uniform location]
          fx-uniforms    (reduce (fn [uniforms [uniform-name value]]
                                   (assoc uniforms
                                          uniform-name
                                          [(atom value)
                                           (with-gl-context gl-lock window capabilities
                                             (log/info "getting location of uniform" uniform-name)
                                             (let [location (GL20/glGetUniformLocation fb-pgm-id (str uniform-name))]
                                               #_(assert (not (neg? location)) (str "Could not find location for uniform " uniform-name location))
                                               (log/info "got location of uniform" uniform-name location)
                                               location))]))
                                 {}
                                 (get fx-shader :uniforms))
          _ (with-gl-context gl-lock window capabilities
              (doseq [idx (range (GL20/glGetProgrami fb-pgm-id GL20/GL_ACTIVE_UNIFORMS))]
                (let [length-buffer (BufferUtils/createIntBuffer 1)
                      size-buffer (BufferUtils/createIntBuffer 1)
                      type-buffer (BufferUtils/createIntBuffer 1)
                      name-buffer (BufferUtils/createByteBuffer 100)
                      uniform-name (GL20/glGetActiveUniform fb-pgm-id idx 256 size-buffer type-buffer)]
                (log/info "Found uniform" uniform-name))))
          key-callback          (proxy [GLFWKeyCallback] []
                                  (invoke [window key scancode action mods]
                                    (when-let [key (zkeyboard/convert-key-code key scancode action mods)]
                                      (log/info "key" key)
                                      (on-key-fn key))))
          char-callback         (proxy [GLFWCharCallback] []
                                  (invoke [window codepoint]
                                    (log/info "char" key)
                                    (on-key-fn (first (Character/toChars codepoint)))))
          mouse-button-callback (proxy [GLFWMouseButtonCallback] []
                                  (invoke [window button action mods]
                                    (let [state  (condp = (int action)
                                                   GLFW/GLFW_PRESS :mouse-down
                                                   GLFW/GLFW_RELEASE :mouse-up)
                                          [col
                                           row]   @mouse-col-row]
                                      (async/put! term-chan (case (int button)
                                        0 {:button :left :type state :col col :row row}
                                        1 {:button :right :type state :col col :row row}
                                        2 {:button :middle :type state :col col :row row}))
                                      (when (and (= state :mouse-up)
                                                 (= [col row] @mousedown-col-row))
                                        (async/put! term-chan {:type :click :col col :row row}))
                                      (reset! mousedown-col-row [col row]))))
          cursor-pos-callback   (proxy [GLFWCursorPosCallback] []
                                  (invoke [window xpos ypos]
                                    #_(let [col (int (quot xpos (int character-width)))
                                          row (int (quot ypos (int character-height)))
                                          [last-col
                                           last-row] @mouse-col-row]
                                      (when (not= [col row] @mouse-col-row)
                                        (when (and (not (nil? last-col))
                                                   (not (nil? last-row)))
                                          (async/put! term-chan {:type :mouse-leave :col last-col :row last-row}))
                                        (reset! mouse-col-row [col row])
                                        (async/put! term-chan {:type :mouse-enter :col col :row row})))))
          terminal
          ;; Create and return terminal
          (OpenGlTerminal. ;columns
                           ;rows
                           ;next-pow-2-columns
                           ;next-pow-2-rows
                           ;font-textures
                           ;normal-font
                           fbo-texture
                           fullscreen
                           antialias
                           layer-character-map-cleared
                           layer-character-map
                           group-map
                           group->font-texture
                           layer-id->group
                           cursor-xy
                           fx-uniforms
                           {:p-matrix-buffer (ortho-matrix-buffer framebuffer-width framebuffer-height)
                            :mv-matrix-buffer (position-matrix-buffer [(- (/ framebuffer-width 2)) (- (/ framebuffer-height 2)) -1.0 0.0]
                                                                      [framebuffer-width framebuffer-height 1.0])
                            :buffers {:vertices-vbo-id vertices-vbo-id
                                      :vertices-count vertices-count
                                      :texture-coords-vbo-id texture-coords-vbo-id
                                      :vao-id vao-id
                                      :fbo-id fbo-id}
                            :textures {:glyph-textures glyph-textures
                                       ;:font-texture font-texture
                                       :fg-textures fg-textures
                                       :bg-textures bg-textures
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
                                       :u-num-layers u-num-layers
                                       :font-size font-size
                                       :term-dim term-dim
                                       :font-tex-dim font-tex-dim
                                       ;:font-texture-width font-texture-width
                                       ;:font-texture-height font-texture-height
                                       :glyph-tex-dim glyph-tex-dim
                                       ;:glyph-texture-width glyph-texture-width
                                       ;:glyph-texture-height glyph-texture-height
                                       :u-fb u-fb
                                       :framebuffer-width framebuffer-width
                                       :framebuffer-height framebuffer-height}
                            :data {:glyph-image-data glyph-image-data
                                   :fg-image-data fg-image-data
                                   :bg-image-data bg-image-data}}
                           term-chan
                           term-pub
                           gl-lock
                           destroyed
                           window
                           capabilities)]
      ;; Access to terminal will be multi threaded. Release context so that other threads can access it)))
      (when @fullscreen
        (zat/fullscreen! terminal (first (zat/fullscreen-sizes terminal))))
      ;; Start font file change listener thread
      ; TODO: fix resource reloader
      #_(cwc/start-watch [{:path "./fonts"
                         :event-types [:modify]
                         :bootstrap (fn [path] (println "Starting to watch " path))
                         :callback (fn [_ filename]
                                     (println "Reloading font" filename)
                                     (reset! normal-font
                                             (make-font filename Font/PLAIN font-size)))
                         :options {:recursive true}}])
      ;; Poll keyboard in background thread and offer input to key-chan
      ;; If gl-lock is false ie: the window has been closed, put :exit on the term-chan
      (reset! key-callback-atom key-callback)
      (reset! char-callback-atom char-callback)
      (reset! mouse-button-callback-atom mouse-button-callback)
      (reset! cursor-pos-callback-atom cursor-pos-callback)
      (GLFW/glfwSetKeyCallback window key-callback)
      (GLFW/glfwSetCharCallback window char-callback)
      (GLFW/glfwSetMouseButtonCallback window mouse-button-callback)
      (GLFW/glfwSetCursorPosCallback window cursor-pos-callback)
      (future
        ;; Wait for Display to be created
        (.await latch)
        (f terminal))
      (loop []
        (.countDown latch)
        (if (with-gl-context gl-lock window capabilities
              (except-gl-errors "Start of loop")
              ; Process messages in the main thread rather than the input go-loop due to Windows only allowing
              ; input on the thread that created the window
              (GLFW/glfwPollEvents)
              ;; Close the display if the close window button has been clicked
              ;; or the gl-lock has been released programmatically (e.g. by destroy!)
              (or (= (GLFW/glfwWindowShouldClose window) GLFW/GLFW_TRUE) @destroyed))
          (do
            (log/info "Destroying display")
            (with-gl-context gl-lock window capabilities
              (reset! gl-lock false)
              ;; TODO: Clean up textures and programs
              ;;(let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id vao-id fbo-id]} :buffers
              ;;       {:keys [font-texture glyph-texture fg-texture bg-texture fbo-texture]} :textures
              ;;       program-id :program-id
              ;;       fb-program-id :fb-program-id} gl]
                (doseq [id [pgm-id fb-pgm-id]]
                  (GL20/glDeleteProgram (int id)))
                (doseq [id (flatten [[fbo-texture] glyph-textures fg-textures bg-textures])]
                  (GL11/glDeleteTextures (int id))))
              (GLFW/glfwDestroyWindow window)
            (log/info "Exiting")
            (on-key-fn :exit))
          (do
            (Thread/sleep 5)
            (recur))))))
      
