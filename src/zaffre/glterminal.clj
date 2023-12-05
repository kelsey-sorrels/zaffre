; Functions for rendering characters to screen
(ns zaffre.glterminal
  (:require [zaffre.terminal :as zt]
            [zaffre.font :as zfont]
            [zaffre.color :as zcolor]
            [zaffre.util :as zutil]
            [zaffre.imageutil :as zimg]
            [zaffre.lwjglutil :as zlwjgl]
            [zaffre.keyboard :as zkeyboard]
            [nio.core :as nio]
            [taoensso.timbre :as log]
            [clojure.core.async :as async]
            [clojure-watch.core :as cwc]
            [clojure.test :refer [is]])
  (:import
    (java.lang.reflect Field)
    (org.lwjgl BufferUtils)
    (java.nio Buffer FloatBuffer ByteBuffer)
    (java.nio.charset Charset)
    (org.lwjgl.opengl GL GLUtil GL11 GL12 GL13 GL14 GL15 GL20 GL30 GL32)
    (org.lwjgl.glfw GLFW GLFWVidMode GLFWVidMode$Buffer GLFWImage GLFWImage$Buffer
      GLFWDropCallback GLFWErrorCallback GLFWCharCallback GLFWCharModsCallback GLFWKeyCallback GLFWMouseButtonCallback
      GLFWCursorPosCallback GLFWFramebufferSizeCallback)
    (org.lwjgl.system Platform)
    (org.joml Matrix4f Vector3f)
    (java.io File FileInputStream FileOutputStream)
    (zaffre.terminal Terminal)
    (zaffre.font CP437Font TTFFont))
  (:gen-class))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; GLFW won't maintain a strong reference to our callback objects, so we have to
(def key-callback-atom (atom nil))
(def char-mods-callback-atom (atom nil))
(def mouse-button-callback-atom (atom nil))
(def cursor-pos-callback-atom (atom nil))
(def drop-callback-atom (atom nil))

(def empty-color-table-texture-image  (zimg/image 0 0))

(defn flip-byte-buffer [^ByteBuffer byte-buffer]
  (let [buffer ^Buffer byte-buffer]
    (.flip buffer)
    byte-buffer))

(defn flip-float-buffer [^FloatBuffer float-buffer]
  (let [buffer ^Buffer float-buffer]
    (.flip buffer)
    float-buffer))

(defn- get-fields [#^Class static-class]
  (.getFields static-class))

(def gl-classes [GL11 GL12 GL13 GL14 GL15 GL20 GL30])

(defn- gl-enum-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name as a string."
  [enum-value]
  (if (zero? enum-value)
    "NONE"
    (.getName #^Field (some
                       #(when (= enum-value (.get #^Field % nil)) %)
                       (mapcat get-fields gl-classes)))))

(defn- gl-errors
  [msg]
  (loop [errors [] error (GL11/glGetError) max-errors 10]
    (if (or (zero? error) (neg? max-errors))
      (not-empty errors)
      (recur (conj errors (str "OpenGL Error(" error "):"
                            (gl-enum-name error) ": " msg " - "
                            (zlwjgl/gl-error-string error)))
             (GL11/glGetError)
             (dec max-errors)))))

(defn- log-gl-errors
  [msg]
  (doseq [e (gl-errors msg)]
    (log/error e)))
 
(defn- except-gl-errors
  [msg]
  (when *assert*
    (when-let [errors (gl-errors msg)]
      (throw (Exception. (str (apply str errors)))))))

(def last-context-thread-id (atom nil))
(def platform (zlwjgl/platform))
(defmacro with-gl-context
  "Executes exprs in an implicit do, while holding the monitor of x and aquiring/releasing the OpenGL context.
  Will release the monitor of x in all circumstances."
  [x w c & body]
  `(let [lockee# ~x
         window# ~w
         capabilities# ~c]
     (locking lockee#
     (try
       (when @lockee#
         (let [thread-id# (.getId (Thread/currentThread))]
           (when (or (not= thread-id# @last-context-thread-id)
                     (= platform :windows))
             (GLFW/glfwMakeContextCurrent window#)
             (GL/setCapabilities capabilities#)
             (reset! last-context-thread-id thread-id#)))
         ~@body)
       (finally
         (log-gl-errors "Errors in context")
         (when @lockee#
           (GLFW/glfwMakeContextCurrent 0)))))))

(defmacro defn-memoized [fn-name & body]
  "Def's a memoized fn. Same semantics as defn."
  `(def ~fn-name (memoize (fn ~@body))))

(defn font-key [font] font)

(defn-memoized gl-enum-value
  ;;"Takes a gl enum value as a keyword and returns the value, eg: :gl-one -> GL11/GL_ONE."
  [gl-keyword]
  (let [field-name (clojure.string/upper-case (clojure.string/replace (name gl-keyword) "-" "_"))]
    (some (fn [#^Class static-class]
            (try
              (.get #^Field (.getField static-class field-name) static-class)
              (catch Throwable t
                nil)))
          gl-classes)))
    
  

(defn- texture-id-2d
  ([{:keys [width height byte-buffer] :as img}]
  (log/info "texture-id-2d" img)
  (let [texture-id     (GL11/glGenTextures)
        ^ByteBuffer byte-buffer byte-buffer]
     (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
     (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
     (GL11/glPixelStorei GL11/GL_UNPACK_ROW_LENGTH 0)
     (GL11/glPixelStorei GL11/GL_UNPACK_SKIP_PIXELS 0)
     (GL11/glPixelStorei GL11/GL_UNPACK_SKIP_ROWS 0)
     (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
     (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
     (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA (long width) (long height) 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE byte-buffer)
     (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
     (except-gl-errors "end of texture-id-2d")
     texture-id)))

(defn- texture-id
  ([{:keys [width height byte-buffer]}]
    (texture-id width height 1 byte-buffer))
  ([width height layers]
   (texture-id width height layers (BufferUtils/createByteBuffer (* width height 4 layers))))
  ([^long width ^long height ^long layers ^ByteBuffer byte-buffer]
   (let [texture-id (GL11/glGenTextures)]
     (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY texture-id)
     (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
     (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
     (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
     (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL11/GL_RGBA width height layers 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE byte-buffer)
     (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY 0)
     (except-gl-errors "end of texture-id")
     texture-id)))

(defn- xy-texture-id [^long width ^long height ^long layers ^ByteBuffer texture-buffer]
  (let [texture-id (GL11/glGenTextures)]
    (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY texture-id)
    (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL30/GL_TEXTURE_2D_ARRAY GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
    (GL12/glTexImage3D GL30/GL_TEXTURE_2D_ARRAY 0 GL30/GL_RGBA8UI width height layers 0 GL30/GL_RGBA_INTEGER GL11/GL_BYTE texture-buffer)
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
  

(defn- fbo-texture 
  ([^long width ^long height]
   (let [fbo-id     (GL30/glGenFramebuffers)]
     (fbo-texture fbo-id width height)))
  ([fbo-id ^long width ^long height]
   (let [texture-id (GL11/glGenTextures)
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
     [fbo-id texture-id])))
        
;; Extract native libs and setup system properties
(defn init-natives []
  (when (.exists (File. "natives"))
  ;(System/setProperty "java.library.path", (.getAbsolutePath (File. "natives")))
  (condp = [(zlwjgl/platform) (.endsWith (System/getProperty "os.arch") "64")]
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

(defn- init-display [title screen-width screen-height icon-paths destroyed]
   ;; init-natives must be called before the Display is created
   (init-natives)
   (GLFW/glfwSetErrorCallback (proxy [GLFWErrorCallback] []
                                (invoke [error description]
                                  (log/error "GLFW error:" error (GLFWErrorCallback/getDescription description)))))
   (when-not (GLFW/glfwInit)
     (assert "Unable to initialize GLFW"))
   (GLFW/glfwDefaultWindowHints)
   ;(GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
   (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
   
   (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
   (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 2)
   (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
   (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GLFW/GLFW_TRUE)
   (if-let [window (GLFW/glfwCreateWindow (int screen-width) (int screen-height) (str title) 0 0)]
     (do
       (let [vidmode (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))
             x       (/ (- (.width vidmode) screen-width) 2)
             y       (/ (- (.height vidmode) screen-height) 2)]
         (GLFW/glfwSetWindowPos window x y)
         ; Include numlock mod when receiving input
         (GLFW/glfwSetInputMode window GLFW/GLFW_LOCK_KEY_MODS 1)
         window))
   (throw (RuntimeException. "Failed to create the GLFW window"))))

(defn- init-fb
  "Makes context current and creates capabilities."
  [window]
  (GLFW/glfwMakeContextCurrent window)
  (GLFW/glfwSwapInterval 0)
  (GLFW/glfwShowWindow window)
  (let [capabilities (GL/createCapabilities)
        width-buffer (BufferUtils/createIntBuffer 1)
        height-buffer (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetFramebufferSize window width-buffer height-buffer)
    (let [framebuffer-width (.get width-buffer)
          framebuffer-height (.get height-buffer)]
      (GL11/glViewport 0 0 framebuffer-width framebuffer-height)
      ; XXX: Opengl debug
      (GLUtil/setupDebugMessageCallback System/out)
      ; XXX: Opengl debug
      (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_DEBUG_CONTEXT GLFW/GLFW_TRUE)
      ;; Signal to parent that display has been created
      [capabilities
       framebuffer-width
       framebuffer-height])))


(defn- video-modes [^GLFWVidMode$Buffer video-modes-buffer]
  (mapv (fn [idx]
          (let [^GLFWVidMode video-mode (.get video-modes-buffer (int idx))]
            {:width (.width video-mode)
             :height (.height video-mode)
             :refresh-rate (.refreshRate video-mode)}))
        (range (.limit video-modes-buffer))))

(defn- glfw-fullscreen-sizes []
  (let [monitors (GLFW/glfwGetMonitors)]
    (vec
      (mapcat (fn [idx]
                (let [monitor      (.get monitors (int idx))
                      monitor-name (GLFW/glfwGetMonitorName monitor)]
                  (mapv (fn [video-mode]
                          (assoc video-mode :name monitor-name :monitor monitor))
                        (video-modes (GLFW/glfwGetVideoModes monitor)))))
              (range (.limit monitors))))))

(defn- glfw-framebuffer-size [window]
  (let [width-buffer (BufferUtils/createIntBuffer 1)
        height-buffer (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetFramebufferSize (long window) width-buffer height-buffer)
    (let [framebuffer-width (.get width-buffer)
          framebuffer-height (.get height-buffer)]
      [framebuffer-width framebuffer-height])))

(defn- glfw-window-size [window]
  (let [width-buffer (BufferUtils/createIntBuffer 1)
        height-buffer (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetWindowSize (long window) width-buffer height-buffer)
    (let [window-width (.get width-buffer)
          window-height (.get height-buffer)]
      [window-width window-height])))

(defn- glfw-monitor-size
  []
  (let [monitor  (GLFW/glfwGetPrimaryMonitor)
        vid-mode (GLFW/glfwGetVideoMode monitor)]
    [(.width vid-mode)
     (.height vid-mode)]))

(defn- glfw-window-video-mode
  [window]
  (let [monitor        (GLFW/glfwGetWindowMonitor window) 
        [width height] (glfw-window-size window)]
  (log/info "glfw-monitor-video-mode" window monitor)
  (cond->
    {:width width
     :height height
     :refresh-rate 0
     :monitor monitor}
    (pos? monitor) (assoc :name (GLFW/glfwGetMonitorName monitor)))))


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
  (^FloatBuffer [viewport-width viewport-height]
    (ortho-matrix-buffer viewport-width viewport-height (BufferUtils/createFloatBuffer 16)))
  (^FloatBuffer [viewport-width viewport-height ^FloatBuffer matrix-buffer]
    ; TODO: use simpler method to construct matrix
    #_(let [ortho-matrix (doto (Matrix4f.)
                             (.ortho2D 0 viewport-width 0 viewport-height))
          matrix-buffer matrix-buffer]
          (.clear matrix-buffer)
          (.get ortho-matrix matrix-buffer))
    (let [ortho-matrix ^Matrix4f (doto (Matrix4f.) (.identity))
          matrix-buffer matrix-buffer
          zNear   10
          zFar   -10
          m00val     (float (/ 2 viewport-width))
          m11val     (float (/ 2 viewport-height))
          m22val     (float (/ -2 (- zFar zNear)))
          m23val     (float (/ (- (+ zFar zNear)) (- zFar zNear)))
          m33val     (float 1)]
      (.m00 ortho-matrix m00val)
      (.m11 ortho-matrix m11val)
      (.m22 ortho-matrix m22val)
      (.m23 ortho-matrix m23val)
      (.m33 ortho-matrix m33val)
      (.clear matrix-buffer)
      (.get ortho-matrix matrix-buffer)
      matrix-buffer)))

(defn position-matrix-buffer
  (^FloatBuffer [v s]
   (position-matrix-buffer v s (BufferUtils/createFloatBuffer 16)))
  (^FloatBuffer [v s ^FloatBuffer matrix-buffer]
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
                                  flip-float-buffer)
        texture-coords-buffer (-> (BufferUtils/createFloatBuffer (count texture-coords))
                                  (.put texture-coords)
                                  flip-float-buffer)
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

(defprotocol IGLCharacter
  (character [this])
  (fg-color [this])
  (bg-color [this])
  (blend-mode [this]))

; style #{:fg-bg ; reverse foreground and background colors
;        }
; blend-mode one of :normal, or :multiply
(defrecord GLCharacter [c fg bg blend-mode]
  Object
  (toString [this]
    (pr-str this))
  IGLCharacter
  (character [this] c)
  (fg-color [this] fg)
  (bg-color [this] bg)
  (blend-mode [this] blend-mode))

(defn make-terminal-character
  ([character fg-color bg-color]
  (make-terminal-character character fg-color, bg-color :normal))
  ([character fg-color bg-color blend-mode]
   ; uncomment for debugging
   #_{:pre [(char? character)
          (vector? fg-color)
          (< 2 (count fg-color) 5)
          (vector? bg-color)
          (< 2 (count bg-color) 5)
          (contains? #{:normal :multiply} blend-mode)]}
   (GLCharacter. character fg-color bg-color blend-mode)))

;; Normally this would be a record, but until http://dev.clojure.org/jira/browse/CLJ-1224 is fixed
;; it is not performant to memoize records because hashCode values are not cached and are recalculated
;; each time.

(deftype OpenGlTerminal [term-args
                           window-size
                           framebuffer-size
                           monitor-fullscreen-sizes
                           group-map
                           group-order
                           group->font-texture
                           group-id->index
                           layer-id->group
                           layer-id->index
                           cursor-xy
                           fx-uniforms
                           gl
                           term-chan
                           term-pub
                           gl-lock
                           destroyed
                           window
                           capabilities]


  clojure.lang.ILookup
  ;; valAt gives (get pm key) and (get pm key not-found) behavior
   (valAt [this item] (get {:term-args                term-args
                            :window-size              window-size
                            :framebuffer-size         framebuffer-size
                            :monitor-fullscreen-sizes monitor-fullscreen-sizes
                            :group-map                group-map
                            :group-order              group-order
                            :group->font-texture      group->font-texture
                            :group-id->index          group-id->index
                            :layer-id->group          layer-id->group
                            :layer-id->index          layer-id->index
                            :cursor-xy                cursor-xy
                            :fx-uniforms              fx-uniforms
                            :gl                       gl
                            :term-chan                term-chan
                            :term-pub                 term-pub
                            :gl-lock                  gl-lock
                            :destroyed                destroyed
                            :window                   window
                            :capabilities             capabilities} item))
   (valAt [this item not-found] (get {:term-args                term-args
                                      :window-size              window-size
                                      :framebuffer-size         framebuffer-size
                                      :monitor-fullscreen-sizes monitor-fullscreen-sizes
                                      :group-map                group-map
                                      :group-order              group-order
                                      :group->font-texture      group->font-texture
                                      :group-id->index          group-id->index
                                      :layer-id->group          layer-id->group
                                      :layer-id->index          layer-id->index
                                      :cursor-xy                cursor-xy
                                      :fx-uniforms              fx-uniforms
                                      :gl                       gl
                                      :term-chan                term-chan
                                      :term-pub                 term-pub
                                      :gl-lock                  gl-lock
                                      :destroyed                destroyed
                                      :window                   window
                                      :capabilities             capabilities} item not-found))

  clojure.lang.IFn
  ;;makes priority map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  zt/Terminal
  (args [_]
    term-args)
  (groups [_]
     (into {} (map (fn [[id ref]]
                     (let [group @ref
                           gg @(get group->font-texture id)]
                       [id (merge group (select-keys gg [:font-texture-width
                                                         :font-texture-height 
                                                         :character-width
                                                         :character-height]))]))
                   group-map)))
  (alter-group-pos! [_ group-id pos-fn]
    (alter (get group-map group-id) (fn [group] (update group :pos (fn [pos] (mapv int (pos-fn pos)))))))
  (alter-group-font! [_ group-id font-fn]
    ;; TODO: alter color table?
    (let [gg (with-gl-context gl-lock window capabilities
               (let [old-font-texture  (-> group->font-texture group-id deref :font-texture)
                     old-color-table-texture  (-> group->font-texture group-id deref :color-table-texture)
                     font         (font-fn (zlwjgl/platform))
                     gg           (if (zfont/glyph-graphics? font)
                                    font
                                    (zfont/glyph-graphics font))
                     _ (log/info "gg" gg)
                     font-texture (or (get gg :font-texture)
                                      (texture-id-2d (get gg :font-texture-image)))
                     color-table-texture (texture-id-2d (or (get gg :color-table-texture-image)
                                                             empty-color-table-texture-image))]
                 ;(log/info "loaded font-texture for" group-id font-texture)
                 ;(log/info "using tile-names" (zfont/character-layout gg))
                 ;(log/info "using character->col-row" (zfont/character->col-row gg))
                 (dosync 
                   (alter (get group->font-texture group-id)
                          assoc :font-texture font-texture
                                :font-texture-width (get gg :font-texture-width)
                                :font-texture-height (get gg :font-texture-height)
                                :character-width (get gg :character-width)
                                :character-height (get gg :character-height)
                                :color-table-texture color-table-texture
                                :character->col-row (zfont/character->col-row gg)))

                 ; Free old texture
                 (log/info "Freeing texture" old-font-texture old-color-table-texture)
                 (GL11/glDeleteTextures (long old-font-texture))
                 (GL11/glDeleteTextures (long old-color-table-texture))
                 (log/info "after")
                 gg))]
       (async/put! term-chan [(select-keys gg [:font-texture-width
                                              :font-texture-height 
                                              :character-width
                                              :character-height])
                              :font-change])))
  ;; characters is a list of {:c \character :x col :y row :fg [r g b [a]] :bg [r g b [a]]}
  (put-chars! [_ layer-id characters]
    {:pre [(is (not (nil? (get layer-id->group layer-id))) (format "Invalid layer-id %s" layer-id))
           (is (coll? characters) "characters must be a collection")
           (every? true? (map (fn [character]
                                (is (every? character #{:x :y :c :fg :bg}) (str character "missing required keys"))
                                (is number? (get character :x))
                                (is number? (get character :y)))
                              characters))]}
    (let [columns (-> layer-id layer-id->group deref :columns)
          rows    (-> layer-id layer-id->group deref :rows)
          group-id (-> layer-id layer-id->group deref :id)
          group-index  (get group-id->index group-id)
          data (:data gl)
          ^ByteBuffer glyph-image-data (get-in data [:glyph-image-data group-index])
          ^ByteBuffer fg-image-data    (get-in data [:fg-image-data group-index])
          ^ByteBuffer bg-image-data    (get-in data [:bg-image-data group-index])
          character->col-row (:character->col-row (-> group-id group->font-texture deref))
          texture-columns (int (zutil/next-pow-2 columns))
          texture-rows    (int (zutil/next-pow-2 rows))
          layer-size      (* 4 texture-columns texture-rows)
          layer-index     (layer-id->index layer-id)]
      (doseq [{:keys [c x y fg bg blend-mode] :or {blend-mode :normal} :as character} characters
              :when (and (< -1 x) (< x columns)
                         (< -1 y) (< y rows)
                         (not= c (char 0)))
              :let [i         (int (+ (* 4 (+ (* texture-columns (- rows y 1)) x)) (* layer-size layer-index)))
                    [x y]     (character->col-row c)
                    fg (cond
                         (integer? fg) fg
                         (vector? fg) (zcolor/color fg))
                    bg (cond
                         (integer? bg) bg
                         (vector? bg) (zcolor/color bg))
                    palette-offset (if-let [palette-offset (:palette-offset character)]
                                     (inc palette-offset)
                                     0)]]
          (when (or (nil? x) (nil? y))
            (log/error (format "X/Y nil - glyph not found for character %s %s"
                         (or (str c) "nil")
                         (or (cond
                               (nil? c) "nil"
                               (char? c) (format "%x" (int c))
                               :else (str c)))))
            (assert
              (format "X/Y nil - glyph not found for character %s %s"
                (or (str c) "nil")
                (or (cond
                      (nil? c) "nil"
                      (char? c) (format "%x" (int c))
                      :else (str c))))))
          (.position glyph-image-data i)
          (.position fg-image-data i)
          (.position bg-image-data i)

          (.put glyph-image-data (unchecked-byte x))
          (.put glyph-image-data (unchecked-byte y))
          (.put glyph-image-data (unchecked-byte (zt/blend-mode->byte blend-mode)))
          (.put glyph-image-data (unchecked-byte palette-offset))
          (.putInt fg-image-data (unchecked-int fg))
          (.putInt bg-image-data (unchecked-int bg)))))
      
  (put-layer! [_ layer-id buffers]
    #_(log/info "put-layer! characters" (count characters))
    (let [columns (:columns (-> layer-id layer-id->group deref))
          rows    (:rows (-> layer-id layer-id->group deref))
          group-id (:id (-> layer-id layer-id->group deref))
          group-index  (group-id->index group-id)
          data (:data gl)
          ^ByteBuffer glyph-image-data (get-in data [:glyph-image-data group-index])
          ^ByteBuffer color-table-image-data (get-in data [:color-table-image-data group-index])
          ^ByteBuffer fg-image-data    (get-in data [:fg-image-data group-index])
          ^ByteBuffer bg-image-data    (get-in data [:bg-image-data group-index])
          character->col-row (:character->col-row (-> group-id group->font-texture deref))
          texture-columns (int (zutil/next-pow-2 columns))
          texture-rows    (int (zutil/next-pow-2 rows))
          layer-size      (* 4 texture-columns texture-rows)
          layer-index     (layer-id layer-id->index)
          num-cols         (int (zt/num-cols buffers))
          num-rows         (int (zt/num-rows buffers))]
      (try
        #_(log/info "put-layer! characters" columns rows group-id group-index layer-index glyph-image-data color-table-image-data fg-image-data bg-image-data)
        #_(log/info "put-layer!" num-cols num-rows)
        #_(log/info "put-layer!" (-> group-id group->font-texture deref))
        #_(log/info "put-layer!" character->col-row)
        #_(log/info (keys gl))
        #_(log/info (keys (get gl :data)))
        (when-not (= rows num-rows)
          (log/error "buffer rows (" num-rows ") does not match group rows (" rows ")"))
        (when-not (= columns num-cols)
          (log/error "buffer columns (" num-cols ") does not match group columns (" columns ")"))
        (when-not glyph-image-data
          (log/error "glyph-image-data nil"))
        (when-not fg-image-data
          (log/error "fg-image-data nil"))
        (when-not bg-image-data
          (log/error "bg-image-data nil"))
        (zutil/loop-range row 0 num-rows
          (let [row (int row)]
            (zutil/loop-range col 0 num-cols
              (let [col (int col)
                    c     (zt/get-char buffers col (- num-rows row 1))
                    [x y] (character->col-row c)
                    blend-mode-byte (zt/get-blend-mode buffers col (- num-rows row 1))
                    fg    (unchecked-int (zt/get-fg buffers col (- num-rows row 1)))
                    bg    (unchecked-int (zt/get-bg buffers col (- num-rows row 1)))]
                (when (and (not= (int c) 0) x y)
                  (let [i (int (+ (* 4 (+ (* texture-columns row) col)) (* layer-size layer-index)))]
                    (when (or (nil? x) (nil? y))
                      (log/error (format "X/Y nil - glyph not found for character %s %s"
                                   (or (str c) "nil")
                                   (or (cond
                                         (nil? c) "nil"
                                         (char? c) (format "%x" (int c))
                                         :else (str c)))))
                      (assert
                        (format "X/Y nil - glyph not found for character %s %s"
                          (or (str c) "nil")
                          (or (cond
                                (nil? c) "nil"
                                (char? c) (format "%x" (int c))
                                :else (str c))))))
                    #_(when false
                      (log/info "put-layer!" col row i x y (Integer/toHexString fg) (Integer/toHexString bg)))

                    ; start of row, copy full row
                    (when (zero? col)
                      (.position glyph-image-data i)
                      (.position fg-image-data i)
                      (.position bg-image-data i)
                      (zt/copy-fg buffers col (- num-rows row 1) num-cols fg-image-data) 
                      (zt/copy-bg buffers col (- num-rows row 1) num-cols bg-image-data))
                    #_(.put (.asIntBuffer fg-image-data) (unchecked-int (zt/get-fg buffers col (- num-rows row 1))))
                    #_(.put (.asIntBuffer bg-image-data) (unchecked-int (zt/get-bg buffers col (- num-rows row 1))))
                    (.put glyph-image-data (unchecked-byte x))
                    (.put glyph-image-data (unchecked-byte y))
                    ;; TODO fill with appropriate type
                    (.put glyph-image-data (unchecked-byte blend-mode-byte))
                    (.put glyph-image-data (unchecked-byte 0))))))))
       (catch Exception e
         (log/error "Error replacing characters in layer" layer-id "=" layer-index
                    "texture-columns" texture-columns "texture-rows" texture-rows glyph-image-data fg-image-data bg-image-data e)
         (log/error e)))))
  (assoc-shader-param! [_ k v]
    (-> fx-uniforms (get k) first (reset! v)))
  (pub [_]
    term-pub)
  (refresh! [_]
    (with-gl-context gl-lock window capabilities
      (let [{{:keys [vertices-vbo-id vertices-count texture-coords-vbo-id vao-id fbo-id]} :buffers
             {:keys [glyph-textures fg-textures bg-textures fbo-texture]} :textures
             program-id :program-id
             fb-program-id :fb-program-id
             {:keys [u-MVMatrix u-PMatrix u-fb-MVMatrix u-fb-PMatrix u-font u-color-table u-glyphs u-fg u-bg u-num-layers font-size term-dim font-tex-dim
                     font-texture-width font-texture-height glyph-tex-dim u-fb]} :uniforms
             {:keys [glyph-image-data
                     fg-image-data
                     bg-image-data]} :data
             :keys [p-matrix-buffer mv-matrix-buffer]} gl
            [framebuffer-width framebuffer-height] @framebuffer-size
            [screen-width screen-height] ((juxt :width :height) @window-size)]
        #_(log/info "drawing with" (mapv vec [glyph-textures fg-textures bg-textures glyph-image-data fg-image-data bg-image-data]))
        ;; Setup render to FBO
        (try
          ;(GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER, fbo-id)
          (GL11/glEnable GL11/GL_BLEND)
          (except-gl-errors "glEnable GL_BLEND")
          (GL11/glDisable GL11/GL_DEPTH_TEST)
          (except-gl-errors "glEnable GL_DEPTH_TEST")
          ;(log/info "glViewport" 0 0 framebuffer-width framebuffer-height)
          (GL11/glViewport 0 0 framebuffer-width framebuffer-height)
          (except-gl-errors (str "glViewport " framebuffer-width framebuffer-height))
          (GL11/glClearColor 0.0 0.0 0.0 0.0)
          (except-gl-errors (str "glClearColor  " 0.0 0.0 1.0 1.0))
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (except-gl-errors (str "glClear  " (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT)))
          (GL20/glUseProgram program-id)
          (GL20/glUniformMatrix4fv (long u-PMatrix) false (ortho-matrix-buffer framebuffer-width framebuffer-height p-matrix-buffer))
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
        ;; Render each group to FBO
        (dosync
          (doseq [[{:keys [id columns rows layers gl-blend-func gl-blend-equation] [x-pos y-pos] :pos}
                   glyph-texture
                   fg-texture
                   bg-texture
                   glyph-image-data
                   fg-image-data
                   bg-image-data] (map
                                    vector
                                    (map (fn [group-id] (deref (get group-map group-id))) group-order)
                                    glyph-textures
                                    fg-textures
                                    bg-textures
                                    glyph-image-data
                                    fg-image-data
                                    bg-image-data)
                  :let [group-id id
                        {:keys [character-width
                                character-height
                                character->transparent
                                font-texture-width
                                font-texture-height
                                font-texture
                                color-table-texture]} (-> group->font-texture group-id deref)
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
            (assert (not (nil? color-table-texture)) "color-table-texture nil")
            (flip-byte-buffer glyph-image-data)
            (flip-byte-buffer fg-image-data)
            (flip-byte-buffer bg-image-data)
            ;(log/info character->transparent)
            (GL14/glBlendEquation (gl-enum-value gl-blend-equation))
            (except-gl-errors "glBlendEquations")
            (GL11/glBlendFunc (gl-enum-value (first gl-blend-func))
                              (gl-enum-value (second gl-blend-func)))
            (except-gl-errors "glBlendFunc")
            #_(doseq [layer (partition layer-size (vec (nio/buffer-seq glyph-image-data)))]
              (log/info "layer")
              (doseq [line (partition (* 4 texture-columns) layer)]
                (log/info (vec line))))
            (try
              (GL20/glUniformMatrix4fv
                (long u-MVMatrix)
                false
                (position-matrix-buffer
                  [(+ x-pos (- (/ framebuffer-width 2)) (quot (- screen-width (* columns character-width)) 2))
                   (+ y-pos (- (/ framebuffer-height 2)) (quot (- screen-height (* rows character-height)) 2))
                   -1.0
                   0.0]
                  [(* columns character-width (/ framebuffer-width screen-width))
                   (* rows character-height (/ framebuffer-height screen-height))
                   1.0]
                  mv-matrix-buffer))
              (except-gl-errors (str "u-MVMatrix - glUniformMatrix4  " u-MVMatrix))
              ; Setup uniforms for font, color-table, glyph, fg, bg, textures
              (GL20/glUniform1i u-font 0)
              (GL20/glUniform1i u-color-table 1)
              (GL20/glUniform1i u-glyphs 2)
              (GL20/glUniform1i u-fg 3)
              (GL20/glUniform1i u-bg 4)
              (GL20/glUniform1i u-num-layers layer-count)
              (except-gl-errors "uniformli bind")
              #_(log/info "drawing" group-id)
              #_(log/info "font-size" character-width character-height)
              #_(log/info "term-dim" columns rows)
              #_(log/info "font-tex-dim" font-texture-width font-texture-height)
              #_(log/info "glyph-tex-dim" texture-columns texture-rows)
              (GL20/glUniform2f font-size character-width character-height)
              (GL20/glUniform2f term-dim columns rows)
              (GL20/glUniform2f font-tex-dim font-texture-width font-texture-height)
              (GL20/glUniform2f glyph-tex-dim texture-columns texture-rows)
              (except-gl-errors "uniform2f bind")
              ; Bind font texture
              #_(log/info "binding font texture" font-texture)
              ; TODO: Find root of font texture update sync bug and remove this check
              (when (GL11/glIsTexture font-texture)
                (GL13/glActiveTexture GL13/GL_TEXTURE0)
                (except-gl-errors "font texture glActiveTexture")
                (GL11/glBindTexture GL11/GL_TEXTURE_2D font-texture)
                (except-gl-errors "font texture glBindTexture"))
              (when (GL11/glIsTexture color-table-texture)
                (GL13/glActiveTexture GL13/GL_TEXTURE1)
                (except-gl-errors "font texture glActiveTexture")
                (GL11/glBindTexture GL11/GL_TEXTURE_2D color-table-texture)
                (except-gl-errors "font texture glBindTexture"))
              ; Send updated glyph texture to gl
              (GL13/glActiveTexture GL13/GL_TEXTURE2)
              (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY glyph-texture)
              (GL12/glTexSubImage3D GL30/GL_TEXTURE_2D_ARRAY 0 0 0 0 texture-columns texture-rows layer-count GL30/GL_RGBA_INTEGER GL11/GL_UNSIGNED_BYTE glyph-image-data)
              (except-gl-errors "glyph texture data")
              ; Send updated fg texture to gl
              (GL13/glActiveTexture GL13/GL_TEXTURE3)
              (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY fg-texture)
              (GL12/glTexSubImage3D GL30/GL_TEXTURE_2D_ARRAY 0 0 0 0 texture-columns texture-rows layer-count GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE fg-image-data)
              (except-gl-errors "fg color texture data")
              ; Send updated bg texture to gl
              (GL13/glActiveTexture GL13/GL_TEXTURE4)
              (except-gl-errors "bg color glActiveTexture")
              (GL11/glBindTexture GL30/GL_TEXTURE_2D_ARRAY bg-texture)
              (except-gl-errors "bg color glBindTexture")
              (GL12/glTexSubImage3D GL30/GL_TEXTURE_2D_ARRAY 0 0 0 0 texture-columns texture-rows layer-count GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bg-image-data)
              (except-gl-errors "bg color glTexImage2D")
              (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
              (except-gl-errors "bg color glDrawArrays")
              (catch Error e
               (log/error "OpenGL error:" e)))
            (.clear glyph-image-data)
            (.clear fg-image-data)
            (.clear bg-image-data)))
        #_(try 
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)

          ;; Draw fbo to screen
          (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
          (GL11/glViewport 0 0 framebuffer-width framebuffer-height)
          (GL11/glClearColor 0.0 0.0 0.0 0.0)
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
          (GL14/glBlendEquation (gl-enum-value :gl-func-add))
          (GL11/glBlendFunc (gl-enum-value :gl-one)
                            (gl-enum-value :gl-one-minus-src-alpha))
          (GL20/glUseProgram fb-program-id)
          (GL20/glUniformMatrix4fv (long u-fb-PMatrix) false (ortho-matrix-buffer framebuffer-width framebuffer-height p-matrix-buffer))
          (except-gl-errors (str "u-fb-PMatrix - glUniformMatrix4  " u-fb-PMatrix))
          (GL20/glUniformMatrix4fv
            (long u-fb-MVMatrix)
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
            (if location
              (if-not (neg? location)
                (GL20/glUniform1f location (-> value-atom deref float))
                (assert false (str "Could not find uniform " uniform-name)))
              (assert false (str "Nil location " uniform-name))))
          ;; Draw fx shaded terminal
          (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 vertices-count)
          ;;;; clean up
          (GL20/glDisableVertexAttribArray 0)
          (GL20/glDisableVertexAttribArray 1)
          (GL20/glUseProgram 0)
          (GL30/glBindVertexArray 0)
          (except-gl-errors "end of refresh")
          (except-gl-errors "end of update")
          (catch Error e
            (log/error "OpenGL error:" e)))
        (GLFW/glfwSwapBuffers window)
        window)))
  (clear! [_]
    (dosync
      (try
        (let [{
               {:keys [glyph-image-data
                       fg-image-data
                       bg-image-data]} :data} gl]
          (doseq [[glyph-image-data
                   fg-image-data
                   bg-image-data] (map
                                    vector
                                    glyph-image-data
                                    fg-image-data
                                    bg-image-data)
                  :let [^ByteBuffer glyph-image-data glyph-image-data
                        ^ByteBuffer fg-image-data fg-image-data
                        ^ByteBuffer bg-image-data bg-image-data]]
            (.clear glyph-image-data)
            (.clear fg-image-data)
            (.clear bg-image-data)
            (BufferUtils/zeroBuffer glyph-image-data)
            (BufferUtils/zeroBuffer fg-image-data)
            (BufferUtils/zeroBuffer bg-image-data)))
        (catch Error e
          (log/error "Error clearing terminal:" e)))))
  (clear! [_ layer-id] nil)
  (set-window-size! [_ v]
    (reset! window-size v))
  (fullscreen-sizes [_]
    monitor-fullscreen-sizes)
  (destroy! [_]
    (reset! destroyed true)
    (async/put! term-chan [nil :close])
    (async/close! term-chan)
    (shutdown-agents))
  (destroyed? [_]
    @destroyed)
  clojure.lang.IHashEq
  (hasheq [this]
    (hash [#_term-args
           window-size
           framebuffer-size
           #_group-map
           #_group-order
           #_group->font-texture
           #_group-id->index
           #_layer-id->group
           #_layer-id->index
           #_destroyed])))

(defmethod zt/do-frame-clear OpenGlTerminal [terminal] (zt/clear! terminal))

(defn group-locations
  ([_ _ _ _]
   [])
  ([group-map group->font-texture framebuffer-width framebuffer-height xpos ypos]
   (set
     (remove (fn [[group-id col row]] (and (nil? col) (nil? row)))
             (map (fn [[group-id group]]
                    (let [{:keys [pos columns rows]
                           [pos-x pos-y] :pos} @group
                          {:keys [character-width character-height]} (-> group->font-texture group-id deref)
                          width  (* character-width columns)
                          height (* character-height rows)]
                      ;; determine if the mouse is in the bounds of the layer group
                      (when (and (<= pos-x xpos (+ (* columns character-width) pos-x))
                               (<= pos-y ypos (+ (* rows character-height) pos-y)))
                        ;; determine which col/row the mouse is in the group
                        (let [col (int (quot (- xpos pos-x (quot (- framebuffer-width width) 2)) character-width))
                              row (int (quot (- ypos pos-y (quot (- framebuffer-height height) 2)) character-height))]
                          [group-id col row]))))
                   group-map)))))

(defn create-terminal
  [groups {:keys [title
                  screen-width 
                  screen-height 
                  default-fg-color 
                  default-bg-color 
                  on-key-fn 
                  fullscreen
                  icon-paths
                  fx-shader]
    :or {title "Zaffre"
         default-fg-color [255 255 255 255]
         default-bg-color [0 0 0 0]
         on-key-fn        nil
         fullscreen       false
         icon-paths       nil
         fx-shader        {:name "passthrough.fs"}} :as opts}
   f]
  (let [term-args           [groups opts]
        default-blend      {:gl-blend-equation :gl-func-add
                            :gl-blend-func [:gl-one :gl-one-minus-src-alpha]}
        group-map           (into {}
                              (map (fn [group]
                                     (is (every? #{:id :layers :columns :rows :pos :font :gl-blend-equation :gl-blend-func} (keys group)))
                                     [(get group :id) (ref (merge default-blend group))])
                                   groups))
        layer-id->index     (into {} (mapcat (fn [group] (map-indexed (fn [index layer-id] [layer-id index]) (get group :layers))) groups))
        group-order         (map :id groups)
        group-id->index     (into {} (map-indexed (fn [index group-id] [group-id index]) group-order))
        layer-id->group     (->> group-map
                               (mapcat (fn [[id layer-group]]
                                         (map (fn [layer-id]
                                                [layer-id layer-group])
                                              (get @layer-group :layers))))
                               (into {}))
        _                        (log/info "group-id->index" group-id->index)
        _                        (log/info "group-map" group-map)
        _                        (log/info "layer-id->group" layer-id->group)
        _                        (log/info "layer-id->index" layer-id->index)
        font-textures       (atom {})
        window-size         (atom nil)
        ;; Last [col row] o f mouse movement
        mouse-xy            (atom nil)
        mousedown-xy        (atom nil)
        ;; false if Display  is destoyed
        destroyed           (atom false)
        gl-lock             (atom true)
        latch               (java.util.concurrent.CountDownLatch. 1)
        window              (init-display title screen-width screen-height icon-paths destroyed)
        [capabilities
         framebuffer-width
         framebuffer-height ]
                            (init-fb window)
        _                   (log/info "window" window "capabilities" capabilities)
        _                   (log/info "screen size" screen-width "x" screen-height)
        _                   (log/info "framebuffer size" framebuffer-width "x" framebuffer-height)
        framebuffer-size    (atom [framebuffer-width framebuffer-height])
        monitor-fullscreen-sizes (glfw-fullscreen-sizes)
        _                   (log/info "monitor-fullscreen-sizes" monitor-fullscreen-sizes)


        _ (except-gl-errors "GLFW init")
        group->font-texture (into {}
                              (mapv (fn [[k v]]
                                      [k (ref (let [font ((get @v :font) (zlwjgl/platform))
                                                    ;_    (log/info "Creating glyph-graphics for" font (zlwjgl/platform))
                                                    gg   (if (zfont/glyph-graphics? font)
                                                           font
                                                           (zfont/glyph-graphics font))
                                                    font-texture (texture-id-2d (get gg :font-texture-image))
                                                    ; 'or' is used here because gg can have the key :color-table-texture-image set to nil
                                                    color-table-texture (texture-id-2d (or (get gg :color-table-texture-image)
                                                                                           empty-color-table-texture-image))]
                                                  (log/info "loaded font-texture for" k font-texture (select-keys gg [:font-texture-width :font-texture-height :character-width :character-height]))
                                                  (log/info "loaded color-table-texture for" k color-table-texture)
                                               (-> gg
                                                 (assoc :font-texture font-texture)
                                                 (assoc :color-table-texture color-table-texture))))])
                                   group-map))
          ;; create empty character maps
          cursor-xy           (atom nil)

          term-chan           (async/chan (async/buffer 100))
          ;; Turn term-chan into a pub(lishable) channel
          term-pub            (async/pub term-chan (fn [[v action]]
                                            ;(log/info "term-chan" v action)
                                            (cond
                                              (zkeyboard/is-keypress? v) :keypress
                                              (keyword? action)          action
                                              (keyword? v)               v)))
                                            
          on-key-fn           (or on-key-fn
                                  (fn alt-on-key-fn [k action]
                                    (async/put! term-chan [k action])))

          ;; create width*height texture that gets updated each frame that determines which character to draw in each cell
          _ (log/info "Creating glyph array")
          
          glyph-image-data    (vec
                                (for [[_ layer-group] group-map
                                      :let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                            np2-rows    (zutil/next-pow-2 (get @layer-group :rows))
                                            np2-layers  (zutil/next-pow-2 (count (get @layer-group :layers)))
                                            buffer (BufferUtils/createByteBuffer (* np2-columns np2-rows 4 np2-layers))]]
                                  (do
                                    (log/info "creating buffer for glyph textures" np2-columns "x" np2-rows "x" np2-layers "x 4" buffer)
                                    buffer)))
          fg-image-data       (vec
                                (for [[_ layer-group] group-map
                                      :let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                            np2-rows    (zutil/next-pow-2 (get @layer-group :rows))
                                            np2-layers  (zutil/next-pow-2 (count (get @layer-group :layers)))
                                            buffer (BufferUtils/createByteBuffer (* np2-columns np2-rows 4 np2-layers))]]
                                  (do
                                    (log/info "creating buffer for fg textures" np2-columns "x" np2-rows "x" np2-layers "x 4" buffer)
                                    buffer)))
          bg-image-data       (vec
                                (for [[_ layer-group] group-map
                                      :let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                            np2-rows    (zutil/next-pow-2 (get @layer-group :rows))
                                            np2-layers  (zutil/next-pow-2 (count (get @layer-group :layers)))
                                            buffer (BufferUtils/createByteBuffer (* np2-columns np2-rows 4 np2-layers))]]
                                  (do
                                    (log/info "creating buffer for bg textures" np2-columns "x" np2-rows "x" np2-layers "x 4" buffer)
                                    buffer)))
          glyph-textures      (mapv (fn [layer-group glyph-image-data]
                                      (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                            np2-rows    (zutil/next-pow-2 (get @layer-group :rows))
                                            np2-layers  (zutil/next-pow-2 (count (get @layer-group :layers)))]
                                        #_(log/info "creating glyph texture" np2-columns "x" np2-rows "x" np2-layers)
                                        (xy-texture-id np2-columns np2-rows np2-layers glyph-image-data)))
                                    (vals group-map)
                                    glyph-image-data)
          fg-textures        (mapv (fn [layer-group fg-image-data]
                                     (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                           np2-rows    (zutil/next-pow-2 (get @layer-group :rows))
                                           np2-layers  (zutil/next-pow-2 (count (get @layer-group :layers)))]
                                       #_(log/info "creating fg texture" np2-columns "x" np2-rows "x" np2-layers)
                                       (texture-id np2-columns np2-rows np2-layers fg-image-data)))
                                   (vals group-map)
                                   fg-image-data)
          bg-textures         (mapv (fn [layer-group bg-image-data]
                                      (let [np2-columns (zutil/next-pow-2 (get @layer-group :columns))
                                            np2-rows    (zutil/next-pow-2 (get @layer-group :rows))
                                            np2-layers  (zutil/next-pow-2 (count (get @layer-group :layers)))]
                                        #_(log/info "creating bg texture" np2-columns "x" np2-rows "x" np2-layers)
                                        (texture-id np2-columns np2-rows np2-layers bg-image-data)))
                                    (vals group-map)
                                    bg-image-data)
          [fbo-id fbo-texture] (fbo-texture framebuffer-width framebuffer-height)
          _ (except-gl-errors "Texture init")
          ; init shaders
          [^int pgm-id
           ^int fb-pgm-id]      (init-shaders (get fx-shader :name))
          _ (except-gl-errors "Shader init")
          pos-vertex-attribute  (GL20/glGetAttribLocation pgm-id "aVertexPosition")
          texture-coords-vertex-attribute    (GL20/glGetAttribLocation pgm-id "aTextureCoord")
          fb-pos-vertex-attribute            (GL20/glGetAttribLocation fb-pgm-id "aVertexPosition")
          fb-texture-coords-vertex-attribute (GL20/glGetAttribLocation fb-pgm-id "aTextureCoord")
          _ (except-gl-errors "Framebuffer init")

          ;; We just need one vertex buffer, a texture-mapped quad will suffice for drawing the terminal.
          {:keys [vertices-vbo-id
                  vertices-count
                  texture-coords-vbo-id
                  vao-id]}   (init-buffers)
          [glyph-tex-dim
           u-MVMatrix
           u-PMatrix
           u-font
           u-color-table
           u-glyphs
           u-fg
           u-bg
           u-num-layers
           font-size
           term-dim
           font-tex-dim]     (mapv #(GL20/glGetUniformLocation pgm-id (str %))
                                   ["glyphTextureDimensions"
                                    "uMVMatrix"
                                    "uPMatrix"
                                    "uFont"
                                    "uColorTable"
                                    "uGlyphs"
                                    "uFg"
                                    "uBg"
                                    "numLayers"
                                    "fontSize"
                                    "termDimensions"
                                    "fontTextureDimensions"])
          [u-fb
           u-fb-MVMatrix
           u-fb-PMatrix]     (mapv #(GL20/glGetUniformLocation fb-pgm-id (str %))
                                   ["uFb"
                                    "uMVMatrix"
                                    "uPMatrix"])
          _ (except-gl-errors "Shader uniforms init")
          ;; map from uniform name (string) to [value atom, uniform location]
          fx-uniforms        (reduce (fn [uniforms [uniform-name value]]
                                       (log/info "getting location of uniform" uniform-name)
                                       (assoc uniforms
                                              uniform-name
                                              [(atom value)
                                               (let [location (GL20/glGetUniformLocation fb-pgm-id (str uniform-name))]
                                                 (assert (not (neg? location)) (str "Could not find location for uniform " uniform-name location))
                                                 (log/info "got location of uniform" uniform-name location)
                                                 location)]))
                                     {}
                                     (get fx-shader :uniforms))
          _                  (doseq [idx (range (GL20/glGetProgrami fb-pgm-id GL20/GL_ACTIVE_UNIFORMS))]
                               (let [length-buffer (BufferUtils/createIntBuffer 1)
                                     size-buffer (BufferUtils/createIntBuffer 1)
                                     type-buffer (BufferUtils/createIntBuffer 1)
                                     name-buffer (BufferUtils/createByteBuffer 100)
                                   uniform-name (GL20/glGetActiveUniform fb-pgm-id idx 256 size-buffer type-buffer)]
                               (log/info "Found uniform" uniform-name)))
        _ (except-gl-errors "Shader fx uniforms init")
        key-callback          (proxy [GLFWKeyCallback] []
                                (invoke [window key scancode action mods]
                                  (let [converted-key (zkeyboard/convert-key-code key scancode action mods)
                                        action (zkeyboard/convert-action action)
                                        key-mods (zkeyboard/key-mods mods)]
                                    (when (and converted-key
                                            (contains? #{:keydown :keyup} action))
                                      (log/info "key-callback" converted-key action mods key-mods)
                                      (when (or (contains? #{:lshift :rshift :lcontrol :rcontrol :lalt :ralt} converted-key)
                                                (= action :keydown))
                                        (on-key-fn converted-key action))))))
        char-mods-callback    (proxy [GLFWCharModsCallback] []
                               (invoke [window codepoint mods]
                                 (let [key-mods (zkeyboard/key-mods mods)
                                       ch (first (Character/toChars codepoint))]
                                   (log/info "char-mods-callback" codepoint mods key-mods)
                                   ; of we got to a number by pressing shift then we're on the numpad
                                   ; and this is handled by key-callback
                                   (when-not (and (<= (int \0) (int ch) (int \9))
                                            (contains? key-mods :shift))
                                     (on-key-fn ch :keypress)))))
        mouse-button-callback (proxy [GLFWMouseButtonCallback] []
                                (invoke [window button action mods]
                                  (let [state  (condp = (int action)
                                                 GLFW/GLFW_PRESS :mouse-down
                                                 GLFW/GLFW_RELEASE :mouse-up)
                                        new-group-locations (apply group-locations group-map group->font-texture
                                                                   framebuffer-width framebuffer-height @mouse-xy)
                                        button (get [:left :right :middle] (int button))]
                                    (async/onto-chan
                                      term-chan
                                      (map (fn [[group-id col row]]
                                             [{:button button :col col :row row :group-id group-id} state])
                                           new-group-locations)
                                      false)
                                    (when (= state :mouse-down)
                                      (reset! mousedown-xy @mouse-xy))
                                    (when (= state :mouse-up)
                                      (async/onto-chan
                                        term-chan
                                        (map (fn [[group-id col row]]
                                               [{:button button :col col :row row :group-id group-id} :click])
                                             (clojure.set/intersection
                                               new-group-locations
                                               (apply group-locations group-map group->font-texture
                                                      framebuffer-width framebuffer-height @mousedown-xy)))
                                        false)))))
        cursor-pos-callback   (proxy [GLFWCursorPosCallback] []
                                (invoke [window xpos ypos]
                                  (let [new-group-locations (group-locations group-map group->font-texture
                                                                             framebuffer-width framebuffer-height
                                                                             xpos ypos)
                                        old-group-locations (apply group-locations group-map group->font-texture
                                                                   framebuffer-width framebuffer-height @mouse-xy)
                                        entered-locations   (clojure.set/difference new-group-locations old-group-locations)
                                        left-locations      (clojure.set/difference old-group-locations new-group-locations)]
                                    (reset! mouse-xy [xpos ypos])
                                    (async/onto-chan
                                      term-chan
                                      (map (fn [[group-id col row]]
                                             [{:col col :row row :group-id group-id} :mouse-leave])
                                           left-locations)
                                      false)
                                    (async/onto-chan
                                      term-chan
                                      (map (fn [[group-id col row]]
                                             [{:col col :row row :group-id group-id} :mouse-enter])
                                           entered-locations)
                                      false))))
        drop-callback         (proxy [GLFWDropCallback] []
                                (invoke [window count names]
                                  (let [names (map #(GLFWDropCallback/getName names %) (range count))]
                                    (log/info "names" (vec names))
                                    (async/onto-chan
                                      term-chan
                                      [[{:names names} :drag-and-drop]]
                                      false))))
        framebuffer-size-callback
                              (proxy [GLFWFramebufferSizeCallback] []
                                (invoke [window framebuffer-width framebuffer-height]
                                  (let [fbo-texture-width  framebuffer-width #_(int (zutil/next-pow-2 framebuffer-width))
                                        fbo-texture-height framebuffer-height #_(int (zutil/next-pow-2 framebuffer-height))
                                        ^ByteBuffer bbnil  nil]
                                    (with-gl-context gl-lock window capabilities
                                      (log/info "Changing size of fbo" fbo-texture)
                                      (let [[screen-width screen-height] ((juxt :width :height) @window-size)]
                                        (log/info "framebuffer size changed" screen-width screen-height fbo-texture-width fbo-texture-height))
                                      (GL11/glBindTexture GL11/GL_TEXTURE_2D fbo-texture)
                                      (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB (long fbo-texture-width) (long fbo-texture-height) 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE bbnil)
                                      (GL11/glBindTexture GL11/GL_TEXTURE_2D 0)
                                      (reset! framebuffer-size [framebuffer-width framebuffer-height])))))
                                   
        gl                 {:p-matrix-buffer (ortho-matrix-buffer framebuffer-width framebuffer-height)
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
                                         :fb-texture-coords-vertex-attribute fb-texture-coords-vertex-attribute}
                            :program-id pgm-id
                            :fb-program-id fb-pgm-id
                            :uniforms {:u-MVMatrix u-MVMatrix
                                       :u-PMatrix u-PMatrix
                                       :u-fb-MVMatrix u-fb-MVMatrix
                                       :u-fb-PMatrix u-fb-PMatrix
                                       :u-font u-font
                                       :u-color-table u-color-table
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
                                       ;:framebuffer-width framebuffer-width
                                       ;:framebuffer-height framebuffer-height}
                                       }
                            :data {:glyph-image-data glyph-image-data
                                   :fg-image-data fg-image-data
                                   :bg-image-data bg-image-data}}
        terminal           ;; Create and return terminal
                           (OpenGlTerminal. term-args
                                            window-size
                                            framebuffer-size
                                            monitor-fullscreen-sizes
                                            group-map
                                            group-order
                                            group->font-texture
                                            group-id->index
                                            layer-id->group
                                            layer-id->index
                                            cursor-xy
                                            fx-uniforms
                                            gl
                                            term-chan
                                            term-pub
                                            gl-lock
                                            destroyed
                                            window
                                            capabilities)]

        (except-gl-errors "Terminal init (end)")
        ;; Release gl context before starting terminal
        (GLFW/glfwMakeContextCurrent 0)
        ;; Access to terminal will be multi threaded. Release context so that other threads can access it
        (when fullscreen
          (zt/set-window-size! terminal (first (zt/fullscreen-sizes terminal))))
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
        (reset! char-mods-callback-atom char-mods-callback)
        (reset! mouse-button-callback-atom mouse-button-callback)
        (reset! cursor-pos-callback-atom cursor-pos-callback)
        (reset! drop-callback-atom drop-callback)
        (GLFW/glfwSetKeyCallback window key-callback)
        (GLFW/glfwSetCharModsCallback window char-mods-callback)
        (GLFW/glfwSetMouseButtonCallback window mouse-button-callback)
        (GLFW/glfwSetCursorPosCallback window cursor-pos-callback)
        (GLFW/glfwSetDropCallback window drop-callback)
        (GLFW/glfwSetFramebufferSizeCallback window framebuffer-size-callback)
        (future
          ;; Wait for main thread loop to start
          (.await latch)
          (try
            (f terminal)
            (catch Throwable t
              (log/error t "Error executing terminal fn")))
          (log/info "done with use supplied fn"))
        (let [[monitor-width
               monitor-height] (with-gl-context gl-lock window capabilities
                                (glfw-monitor-size))
              last-video-mode (atom (with-gl-context gl-lock window capabilities
                                       (glfw-window-video-mode window)))]
          (log/info "current-video-mode" @last-video-mode)
          (reset! window-size @last-video-mode)
          (loop []
            (.countDown latch)
            (if (with-gl-context gl-lock window capabilities
                  (except-gl-errors "Start of loop")
                  ; Process messages in the main thread rather than the input go-loop due to Windows only allowing
                  ; input on the thread that created the window
                  (GLFW/glfwPollEvents)
                  ;; Close the display if the close window button has been clicked
                  ;; or the gl-lock has been released programmatically (e.g. by destroy!)
                  (or (GLFW/glfwWindowShouldClose window) @destroyed))
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
                  (zt/destroy! terminal)
                (log/info "Exiting"))
              (do
                (when-not (= @last-video-mode @window-size)
                  (log/info "window change. Old size" @last-video-mode "New size" @window-size)
                  (with-gl-context gl-lock window capabilities
                    (cond
                      ;; fullscreen mode
                      (pos? (get @window-size :monitor 0))
                      (let [{:keys [width height refresh-rate monitor]} @window-size]
                        (log/info "setting window monitor fullscreen" window monitor 0 0 width height refresh-rate)
                        (GLFW/glfwSetWindowMonitor window
                                                   monitor
                                                   0
                                                   0
                                                   width
                                                   height
                                                   refresh-rate))
                      ;; fullscren -> windowed mode
                      (pos? (get @last-video-mode :monitor 0))
                      (let [{:keys [width height]} @window-size]
                        (log/info "setting window monitor windowed from fullscreen" window 0 0 0 width height 0)
                        (GLFW/glfwSetWindowMonitor window
                                                   (long 0)
                                                   (int 0)
                                                   (int 0)
                                                   (int width)
                                                   (int height)
                                                   (int 0))
                        (GLFW/glfwSetWindowPos window (quot (- monitor-width width) 2)
                                                      (quot (- monitor-height height) 2)))
                      :else
                      (let [{:keys [width height]} @window-size]
                        (log/info "setting window monitor windowed from windowed" window 0 0 0 width height 0)
                        (GLFW/glfwSetWindowSize window width height)
                        (GLFW/glfwSetWindowPos window (quot (- monitor-width width) 2)
                                                      (quot (- monitor-height height) 2))))
                    (reset! last-video-mode @window-size)))
                ;(GLFW/glfwWaitEvents)
                (Thread/sleep 1)
                (recur)))))))
      
