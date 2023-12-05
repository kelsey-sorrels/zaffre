;; Copied from https://github.com/rogerallen/hello_lwjgl/blob/master/project.clj
(require 'leiningen.core.eval)

;; per-os jvm-opts code cribbed from Overtone
(def JVM-OPTS
  {:common   []
   :macosx   ["-XstartOnFirstThread" "-Djava.awt.headless=true"]
   :linux    []
   :windows  []})

(defn jvm-opts
  "Return a complete vector of jvm-opts for the current os."
  [] (let [os (leiningen.core.eval/get-os)]
       (vec (set (concat (get JVM-OPTS :common)
                         (get JVM-OPTS os))))))

(def LWJGL_NS "org.lwjgl")

;; Edit this to change the version.
(def LWJGL_VERSION "3.1.5")

;; Edit this to add/remove packages.
(def LWJGL_MODULES ["lwjgl"
                    "lwjgl-assimp"
                    "lwjgl-bgfx"
                    "lwjgl-egl"
                    "lwjgl-glfw"
                    "lwjgl-jawt"
                    "lwjgl-jemalloc"
                    "lwjgl-lmdb"
                    "lwjgl-lz4"
                    "lwjgl-nanovg"
                    "lwjgl-nfd"
                    "lwjgl-nuklear"
                    "lwjgl-odbc"
                    "lwjgl-openal"
                    "lwjgl-opencl"
                    "lwjgl-opengl"
                    "lwjgl-opengles"
                    "lwjgl-openvr"
                    "lwjgl-par"
                    "lwjgl-remotery"
                    "lwjgl-rpmalloc"
                    "lwjgl-sse"
                    "lwjgl-stb"
                    "lwjgl-tinyexr"
                    "lwjgl-tinyfd"
                    "lwjgl-tootle"
                    "lwjgl-vulkan"
                    "lwjgl-xxhash"
                    "lwjgl-yoga"
                    "lwjgl-zstd"])

;; It's safe to just include all native dependencies, but you might
;; save some space if you know you don't need some platform.
(def LWJGL_PLATFORMS ["linux" "macos" "windows"])

;; These packages don't have any associated native ones.
(def no-natives? #{"lwjgl-egl" "lwjgl-jawt" "lwjgl-odbc"
                   "lwjgl-opencl" "lwjgl-vulkan"})

(defn lwjgl-deps-with-natives []
  (apply concat
         (for [m LWJGL_MODULES]
           (let [prefix [(symbol LWJGL_NS m) LWJGL_VERSION]]
             (into [prefix]
                   (if (no-natives? m)
                     []
                     (for [p LWJGL_PLATFORMS]
                       (into prefix [:classifier (str "natives-" p)
                                     :native-prefix ""]))))))))

(def all-dependencies
  (into ;; Add your non-LWJGL dependencies here
   '[[org.clojure/clojure "1.9.0"]
     [org.clojure/core.cache "0.7.1"]
     [org.clojure/core.async "0.4.474"]
     [commons-io/commons-io "2.6"]
     [overtone/at-at "1.2.0"]
     [kelsey-sorrels/tinter "0.1.1"]
     [rockpick "0.1.0"]
     [nio "1.0.4"]
     [clj-http "3.9.1"]
     [org.joml/joml "1.9.11"]
     [im.bci/pngdecoder "0.13"]
     [com.taoensso/timbre "4.10.0"]
     [clojure-watch "0.1.14"]]
    (lwjgl-deps-with-natives)))

(defproject zaffre "0.4.0-b4"
  :description "A fast Clojure console library"
  :url "https://github.com/kelsey-sorrels/zaffre"
  :license {:name "The MIT License (MIT)"
            :url "https://raw.githubusercontent.com/kelsey-sorrels/zaffre/master/LICENSE"}
  :dependencies ~all-dependencies
  :lein-release {:deploy-via :lein-deploy
                 :scm :git
                 :build-uberjar true}
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ~(if (-> (System/getProperty "os.name")
                    (.toLowerCase)
                    (.contains "mac"))
              ["-XstartOnFirstThread"
               "-Dorg.lwjgl.opengl.Display.enableHighDPI=true"
               #_"-Djava.library.path=native/"]
              ["-Dclojure.debug=true"
               #_"-Djava.library.path=native/"]))
