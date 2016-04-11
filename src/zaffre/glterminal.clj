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
    (org.lwjgl.opengl GL GLUtil GL11 GL12 GL13 GL15 GL20 GL30 GL32)
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
(def mouse-callback-atom (atom nil))

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
     ;;(.order texture-buffer (ByteOrder/nativeOrder))
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
     ;;(.order texture-buffer (ByteOrder/nativeOrder))
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
    ;;(.order texture-buffer (ByteOrder/nativeOrder))
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
        
;; Extract native libs and setup system properties
(defn init-natives []
  (when (.exists (File. "natives"))
  ;(System/setProperty "java.library.path", (.getAbsolutePath (File. "natives")))
  (condp = [(Platform/get) (.endsWith (System/getProperty "os.arch") "64")]
    [Platform/LINUX false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/linux/x86")))
    [Platform/LINUX true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/linux/x86_64")))
    [Platform/MACOSX false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/macosx/x86")))
    [Platform/MACOSX true]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/macosx/x86_64")))
    [Platform/WINDOWS false]
      (System/setProperty "org.lwjgl.librarypath", (.getAbsolutePath (File. "natives/windows/x86")))
    [Platform/WINDOWS true]
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
     ;(future
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
       (if-let [window (GLFW/glfwCreateWindow screen-width screen-height title 0 0)]
         (do
           ;(Display/setDisplayMode (DisplayMode. screen-width screen-height))
           ;(Display/setTitle title)
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
             (let [capabilities (GL/createCapabilities)]
               (GL11/glViewport 0 0 screen-width screen-height)
               ;(GL11/glClearColor 0.0 0.0 1.0 1.0)
               ;(except-gl-errors (str "glClearColor  " 0.0 0.0 1.0 1.0))
               ;(GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
               ;(GLFW/glfwSwapBuffers window)
               ;; Release the Display so that any thread can aquire it including this thread - the thread that
               ;; created it.
               ;(Display/releaseContext)
               (GLFW/glfwMakeContextCurrent 0)
               ;; Signal to parent that display has been created
               [window capabilities])))
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
    #_(let [ortho-matrix (doto (Matrix4f.)
                             (.ortho2D 0 viewport-width 0 viewport-height))
          matrix-buffer matrix-buffer]
          (.clear matrix-buffer)
          (.get ortho-matrix matrix-buffer)
          (.flip matrix-buffer))
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
      ;(.flip matrix-buffer)
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
      ;(.flip matrix-buffer)
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
                           fullscreen
                           antialias
                           character-map-cleared
                           layers-character-map
                           layer-order
                           cursor-xy
                           fx-uniforms
                           gl
                           key-chan
                           mouse-chan
                           gl-lock
                           destroyed
                           window
                           capabilities]
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
  (get-mouse-chan [_]
    mouse-chan)
  (apply-font! [_ windows-font else-font]
    (with-gl-context gl-lock window capabilities
      (reset! normal-font
              (if (= (Platform/get) Platform/WINDOWS)
                windows-font
                else-font))
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
          ;; TODO: fix
          #_(when-not @fullscreen
            (Display/setDisplayMode (DisplayMode. screen-width screen-height)))
          ;; resize FBO
          (GL11/glBindTexture GL11/GL_TEXTURE_2D fbo-texture)
          (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB (int screen-width) (int screen-height) 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE bbnil)
          (swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture (texture-id-2d font-texture-image))))
          (catch Throwable t
            (log/error "Error changing font" t))))))
  (set-cursor! [_ x y]
    (reset! cursor-xy [x y]))
  (refresh! [_]
    (with-gl-context gl-lock window capabilities
      (let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id vao-id fbo-id]} :buffers
             {:keys [font-texture glyph-texture fg-texture bg-texture fbo-texture]} :textures
             program-id :program-id
             fb-program-id :fb-program-id
             {:keys [u-MVMatrix u-PMatrix u-fb-MVMatrix u-fb-PMatrix u-font u-glyphs u-fg u-bg u-num-layers font-size term-dim font-tex-dim
                     font-texture-width font-texture-height glyph-tex-dim glyph-texture-width glyph-texture-height
                     u-fb]} :uniforms
             {:keys [^ByteBuffer glyph-image-data
                     ^ByteBuffer fg-image-data
                     ^ByteBuffer bg-image-data]} :data
             :keys [p-matrix-buffer mv-matrix-buffer]} gl
            glyph-image-data glyph-image-data
            fg-image-data fg-image-data
            bg-image-data bg-image-data
            {:keys [screen-width
                    screen-height
                    character-width
                    character-height
                    character->col-row
                    character->transparent
                    font-texture-width
                    font-texture-height
                    font-texture]} (get @font-textures (font-key @normal-font))
            [display-width display-height] (let [mode (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))]
                                             [(.width mode) (.height mode)])
            base-layer-id (first layer-order)
            layer-size    (* 4 glyph-texture-width glyph-texture-height)]
        (assert (not (nil? font-texture-width)) "font-texture-width nil")
        (assert (not (nil? font-texture-height)) "font-texture-height")
        (assert (not (nil? font-texture)) "font-texture nil")
        ;; Update glyph texture in buffers
        (.clear glyph-image-data)
        (.clear fg-image-data)
        (.clear bg-image-data)
        
        ;(log/info "rendering layers")
        ;(log/info character->transparent)
        (loop-with-index layer [[layer-id character-map] layers-character-map]
        ;;(doseq [[layer-id character-map] layers-character-map]
        ;;  (doseq [[row line] (map-indexed vector (reverse @character-map))
        ;;          [col c]    (map-indexed vector line)]
            ;;(log/info "row" row "col" col "c" c)
        ;(log/info layer-id)
        (loop-with-index row [line (reverse @character-map)]
          (loop-with-index col [c line]
            (let [chr        (or (get c :fx-character) (get c :character))
                  #_#_chr        (if (and (= layer-id base-layer-id)
                                      (char? chr)
                                      (= (char chr) (char 0)))
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
                  i         (+ (* 4 (+ (* glyph-texture-width row) col)) (* layer-size layer))
                  [x y]     (get character->col-row chr)
                  transparent (get character->transparent chr)]
              ;(log/info "Drawing at col row" col row "character from atlas col row" x y c "(index=" i ")")
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
                ;; not base layer and space ie empty, skip forward
                (do
                  (.put glyph-image-data (byte-array 4))
                  (.put fg-image-data (byte-array 4))
                  (.put bg-image-data (byte-array 4)))))))
          #_(log/info "At pos" (.position glyph-image-data))
          #_(log/info "Setting layer" layer "new pos" (* glyph-texture-width glyph-texture-height 4 (inc layer)))
          (.position glyph-image-data (* glyph-texture-width glyph-texture-height 4 (inc layer)))
          (.position fg-image-data    (* glyph-texture-width glyph-texture-height 4 (inc layer)))
          (.position bg-image-data    (* glyph-texture-width glyph-texture-height 4 (inc layer))))
        #_(.position glyph-image-data (.limit glyph-image-data))
        #_(.position fg-image-data (.limit fg-image-data))
        #_(.position bg-image-data (.limit bg-image-data))
        (.flip glyph-image-data)
        (.flip fg-image-data)
        (.flip bg-image-data)
        #_(doseq [layer (partition layer-size (vec (nio/buffer-seq glyph-image-data)))]
          (log/info "layer")
          (doseq [line (partition (* 4 glyph-texture-width) layer)]
            (log/info (vec line))))
        (try
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER, fbo-id)
          (GL11/glViewport 0 0 screen-width screen-height)
          (except-gl-errors (str "glViewport " screen-width screen-height))
          (GL11/glClearColor 0.0 0.0 1.0 1.0)
          (except-gl-errors (str "glClearColor  " 0.0 0.0 1.0 1.0))
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (except-gl-errors (str "glClear  " (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT)))
          (GL20/glUseProgram program-id)
          (GL20/glUniformMatrix4fv u-PMatrix false (ortho-matrix-buffer screen-width screen-height p-matrix-buffer))
          (except-gl-errors (str "u-PMatrix - glUniformMatrix4  " u-PMatrix))
          (GL20/glUniformMatrix4fv u-MVMatrix false (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
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
          ;; TODO: use real value
          (GL20/glUniform1i u-num-layers (count layer-order))
          (except-gl-errors "uniformli bind")
          (GL20/glUniform2f font-size, character-width character-height)
          (GL20/glUniform2f term-dim columns rows)
          (GL20/glUniform2f font-tex-dim font-texture-width font-texture-height)
          (GL20/glUniform2f glyph-tex-dim glyph-texture-width glyph-texture-height)
          (except-gl-errors "uniform2f bind")
          (except-gl-errors "gl(en/dis)able")
          ; Send updated glyph texture to gl
          (GL13/glActiveTexture GL13/GL_TEXTURE1)
          (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY glyph-texture)
          (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL30/GL_RGBA8UI texture-columns texture-rows (count layer-order) 0 GL30/GL_RGBA_INTEGER GL11/GL_UNSIGNED_BYTE glyph-image-data)
          (except-gl-errors "glyph texture data")
          ; Send updated fg texture to gl
          (GL13/glActiveTexture GL13/GL_TEXTURE2)
          (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY fg-texture)
          (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL11/GL_RGBA texture-columns texture-rows (count layer-order) 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE fg-image-data)
          (except-gl-errors "fg color texture data")
          ; Send updated bg texture to gl
          (GL13/glActiveTexture GL13/GL_TEXTURE3)
          (except-gl-errors "bg color glActiveTexture")
          (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY bg-texture)
          (except-gl-errors "bg color glBindTexture")
          (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL11/GL_RGBA texture-columns texture-rows (count layer-order) 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bg-image-data)
          (except-gl-errors "bg color glTexImage2D")
          (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
          (except-gl-errors "bg color glDrawArrays")
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)

          ;; Draw fbo to screen
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
          (GL11/glViewport 0 0 screen-width screen-height)
          (GL11/glClearColor 0.0 1.0 0.0 1.0)
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (GL20/glUseProgram fb-program-id)
          (GL20/glUniformMatrix4fv u-fb-PMatrix false (ortho-matrix-buffer screen-width screen-height p-matrix-buffer))
          (except-gl-errors (str "u-fb-PMatrix - glUniformMatrix4  " u-fb-PMatrix))
          (GL20/glUniformMatrix4fv u-fb-MVMatrix false (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
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
    (doseq [[_ character-map] layers-character-map]
      (ref-set character-map character-map-cleared)))
  (clear! [_ layer-id]
    {:pre [(contains? (set layer-order) layer-id)]}
    (ref-set (get layers-character-map layer-id) character-map-cleared))
  (fullscreen! [_ v]
    (with-gl-context gl-lock window capabilities
      (if (false? v)
        (let [{:keys [screen-width screen-height]} (get @font-textures (font-key @normal-font))]
          (reset! fullscreen false)
          #_(Display/setDisplayMode (DisplayMode. screen-width screen-height)))
        (let [[width height mode] v]
          (reset! fullscreen true)
          #_(Display/setDisplayMode mode)
          #_(Display/setFullscreen true)))))
  (fullscreen-sizes [_]
    (with-gl-context gl-lock window capabilities
      (let [desktop-mode (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))
            modes (GLFW/glfwGetVideoModes (GLFW/glfwGetPrimaryMonitor))
            compatible-modes (filter (fn [^GLFWVidMode mode]
                                       (and (= (.getBitsPerPixel mode) (.getBitsPerPixel desktop-mode))
                                            (= (.getFrequency mode) (.getFrequency desktop-mode))))
                                     (map (fn [i] (.get modes i)) (range (.count modes))))]
        (mapv (fn [^GLFWVidMode mode] [(.getWidth mode)
                          (.getHeight mode)
                          mode])
             compatible-modes))))
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
  [layer-order {:keys [title columns rows default-fg-color default-bg-color on-key-fn windows-font else-font font-size fullscreen antialias icon-paths fx-shader]
    :or {title "Zaffre"
         columns 80
         rows    24
         default-fg-color [255 255 255]
         default-bg-color [0 0 0]
         on-key-fn        nil
         windows-font     (TTFFont. "Courier New" 16 false)
         else-font        (TTFFont. "Monospaced" 16 false)
         fullscreen       false
         antialias        true
         icon-paths       nil
         fx-shader        {:name "passthrough.fs"}}}
   f]
    (let [is-windows       (>= (.. System (getProperty "os.name" "") (toLowerCase) (indexOf "win")) 0)
          normal-font      (atom nil)
          font-textures    (atom {})
          fullscreen       (atom fullscreen)
          antialias        (atom antialias)
          _                (add-watch
                             normal-font
                             :font-watcher
                             (fn [_ _ _ new-font]
                               (log/info "Using font" new-font)
                               (let [;; create texture atlas as Buffered texture
                                     {:keys [font-texture-width
                                             font-texture-height
                                             font-texture-image
                                             character-width
                                             character-height
                                             character->col-row
                                             character->transparent]
                                      :as font-parameters} (zfont/glyph-graphics new-font)
                                     _ (log/info "cw" character-width "cols" columns)
                                     screen-width (* character-width columns)
                                     screen-height (* character-height rows)]
                                 ;(log/info "Created font texture " (dissoc font-parameters :character->col-row) " screen-width" screen-width "screen-height" screen-height)
                                 (log/info "Created font texture " font-parameters " screen-width" screen-width "screen-height" screen-height)
                                 (swap! font-textures assoc
                                                     (font-key new-font)
                                                     {:screen-width screen-width
                                                      :screen-height screen-height
                                                      :character-width character-width
                                                      :character-height character-height
                                                      :character->col-row character->col-row
                                                      :character->transparent character->transparent
                                                      :font-texture-width font-texture-width
                                                      :font-texture-height font-texture-height
                                                      :font-texture-image font-texture-image}))))
          _                  (reset! normal-font (if is-windows
                                                   windows-font
                                                   else-font))
          ;; Last [col row] of mouse movement
          mouse-col-row      (atom nil)
          ;; false if Display is destoyed
          destroyed          (atom false)
          gl-lock            (atom true)
          latch              (java.util.concurrent.CountDownLatch. 1)
          {:keys [screen-width
                  screen-height
                  character-width
                  character-height
                  font-texture-width
                  font-texture-height
                  font-texture-image]} (get @font-textures (font-key @normal-font))
          _                  (log/info "screen size" screen-width "x" screen-height)
          [window capabilities] (init-display title screen-width screen-height icon-paths gl-lock destroyed)
          _                  (log/info "window" window "capabilities" capabilities)

          font-texture       (with-gl-context gl-lock window capabilities (texture-id-2d font-texture-image))
          _                  (swap! font-textures update (font-key @normal-font) (fn [m] (assoc m :font-texture font-texture)))
          ;; create texture atlas
          character-map-cleared (vec (repeat rows (vec (repeat columns (make-terminal-character (char 0) default-fg-color default-bg-color #{})))))
          _                     (log/info "layer-order" layer-order)
          layers-character-map  (zipmap layer-order (repeatedly #(ref character-map-cleared)))
          cursor-xy             (atom nil)

          key-chan         (async/chan)
          mouse-chan       (async/chan (async/buffer 100))
          on-key-fn        (or on-key-fn
                               (fn alt-on-key-fn [k]
                                 (async/put! key-chan k)))

          ;; create width*height texture that gets updated each frame that determines which character to draw in each cell
          _ (log/info "Creating glyph array")
          next-pow-2-columns (zutil/next-pow-2 columns)
          next-pow-2-rows    (zutil/next-pow-2 rows)
          glyph-texture-width  next-pow-2-columns
          glyph-texture-height next-pow-2-rows
          _ (log/info "creating buffers for glyph/fg/bg textures (" next-pow-2-columns "x" next-pow-2-rows ")")
          ;glyph-array    (ta/unsigned-int8 (repeat (* columns rows) 0))
          glyph-image-data (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4 (count layer-order)))
          fg-image-data    (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4 (count layer-order)))
          bg-image-data    (BufferUtils/createByteBuffer (* next-pow-2-columns next-pow-2-rows 4 (count layer-order)))
          glyph-texture    (with-gl-context gl-lock window capabilities (xy-texture-id next-pow-2-columns next-pow-2-rows (count layer-order) glyph-image-data))
          fg-texture       (with-gl-context gl-lock window capabilities (texture-id next-pow-2-columns next-pow-2-rows (count layer-order) fg-image-data))
          ;fg-texture       (texture-id next-pow-2-columns next-pow-2-rows (count layer-order) fg-image-data)
          bg-texture       (with-gl-context gl-lock window capabilities (texture-id next-pow-2-columns next-pow-2-rows (count layer-order) bg-image-data))
          [fbo-id fbo-texture]
                           (with-gl-context gl-lock window capabilities (fbo-texture screen-width screen-height))
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
                                               #_(assert (not (neg? jocation)) (str "Could not find location for uniform " uniform-name location))
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
          key-callback (proxy [GLFWKeyCallback] []
                                        (invoke [window key scancode action mods]
                                          (when-let [key (zkeyboard/convert-key-code key scancode action mods)]
                                            (log/info "key" key)
                                            (on-key-fn key))))
          char-callback (proxy [GLFWCharCallback] []
                                        (invoke [window codepoint]
                                          (log/info "char" key)
                                          (on-key-fn (first (Character/toChars codepoint)))))
          terminal
          ;; Create and return terminal
          (OpenGlTerminal. columns
                           rows
                           next-pow-2-columns
                           next-pow-2-rows
                           font-textures
                           normal-font
                           fbo-texture
                           fullscreen
                           antialias
                           character-map-cleared
                           layers-character-map
                           layer-order
                           cursor-xy
                           fx-uniforms
                           {:p-matrix-buffer (ortho-matrix-buffer screen-width screen-height)
                            :mv-matrix-buffer (position-matrix-buffer [(- (/ screen-width 2)) (- (/ screen-height 2)) -1.0 0.0]
                                                                      [screen-width screen-height 1.0])
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
                                       :u-num-layers u-num-layers
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
                           mouse-chan
                           gl-lock
                           destroyed
                           window
                           capabilities)]
      ;; Access to terminal will be multi threaded. Release context so that other threads can access it)))
      ;(Display/releaseContext)
      (when @fullscreen
        (zat/fullscreen! terminal (first (zat/fullscreen-sizes terminal))))
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
      (reset! key-callback-atom key-callback)
      (reset! char-callback-atom char-callback)
      (GLFW/glfwSetKeyCallback window key-callback)
      (GLFW/glfwSetCharCallback window char-callback)
      #_(go-loop []
        (with-gl-context gl-lock window capabilities
          (try
            (loop []
              (when (Keyboard/next)
                (when (Keyboard/getEventKeyState)
                  (let [character (Keyboard/getEventCharacter)
                        key       (Keyboard/getEventKey)]
                    (convert-key-code character key on-key-fn)))
                (recur)))
            (catch Exception e
              (log/error "Error getting keyboard input" e)))
          (try
            (loop []
              (when (Mouse/next)
                (let [button (Mouse/getEventButton)
                      state  (if (Mouse/getEventButtonState) :mousedown :mouseup)
                      event-x ^int (Mouse/getEventX)
                      event-y ^int (Mouse/getEventY)
                      {:keys [screen-height
                              character-width
                              character-height]} (get @font-textures (font-key @normal-font))
                      col    (quot event-x (int character-width))
                      row    (quot (- (int screen-height) event-y) (int character-height))]
                  (when (<= 0 button 2)
                    (async/put! mouse-chan (case button
                      0 {:button :left :state state :col col :row row}
                      1 {:button :right :state state :col col :row row}
                      2 {:button :middle :state state :col col :row row}))))
                (recur)))
            (catch Exception e
              (log/error "Error getting mouse input" e)))
          (try
            (let [{:keys [screen-height
                          character-width
                          character-height]} (get @font-textures (font-key @normal-font))
                  mouse-x ^int (Mouse/getX)
                  mouse-y ^int (Mouse/getY)
                  col     (quot mouse-x (int character-width))
                  row     (quot (- (int screen-height) mouse-y) (int character-height))]
              (when (not= [col row] @mouse-col-row)
                (async/>! mouse-chan {:mouse-leave @mouse-col-row})
                (reset! mouse-col-row [col row])
                (async/>! mouse-chan {:mouse-enter @mouse-col-row})))
            (catch Throwable e
              (log/error "Error getting mouse movement" e))))
        (if @gl-lock
          (do
            (Thread/sleep 5)
            (recur))
          (on-key-fn :exit)))
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
              ;(Display/processMessages)
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
                  (GL20/glDeleteProgram id))
                (doseq [id [font-texture glyph-texture fg-texture bg-texture fbo-texture]]
                  (GL11/glDeleteTextures id)))
              (GLFW/glfwDestroyWindow window)
            (log/info "Exiting")
            (on-key-fn :exit))
          (do
            (Thread/sleep 5)
            (recur))))))
      
