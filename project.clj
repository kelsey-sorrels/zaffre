(defproject zaffre "0.4.0-SNAPSHOT"
  :description "A fast Clojure console library"
  :url "https://github.com/kelsey-sorrels/zaffre"
  :license {:name "The MIT License (MIT)"
            :url "https://raw.githubusercontent.com/kelsey-sorrels/zaffre/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.3.465"]
                 ;[kelsey-sorrels/lwjgl "3.0.0rc1"]
                 [org.lwjgl/lwjgl"3.1.5"]
                 [org.lwjgl/lwjgl"3.1.5" :classifier "natives-macos"]
                 [org.lwjgl/lwjgl-opengl "3.1.5"]
                 [org.lwjgl/lwjgl-opengl "3.1.5" :classifier "natives-linux"]
                 [org.lwjgl/lwjgl-opengl "3.1.5" :classifier "natives-macos"]
                 [org.lwjgl/lwjgl-opengl "3.1.5" :classifier "natives-windows"]
                 [org.lwjgl/lwjgl-glfw "3.1.5"]
                 [org.lwjgl/lwjgl-glfw "3.1.5" :classifier "natives-linux"]
                 [org.lwjgl/lwjgl-glfw "3.1.5" :classifier "natives-macos"]
                 [org.lwjgl/lwjgl-glfw "3.1.5" :classifier "natives-windows"]
                 [org.lwjgl/lwjgl-stb "3.1.5"]
                 [org.lwjgl/lwjgl-stb "3.1.5" :classifier "natives-linux"]
                 [org.lwjgl/lwjgl-stb "3.1.5" :classifier "natives-macos"]
                 [org.lwjgl/lwjgl-stb "3.1.5" :classifier "natives-windows"]
                 [org.lwjgl/lwjgl-yoga "3.1.5"]
                 [org.lwjgl/lwjgl-yoga "3.1.5" :classifier "natives-linux"]
                 [org.lwjgl/lwjgl-yoga "3.1.5" :classifier "natives-macos"]
                 [org.lwjgl/lwjgl-yoga "3.1.5" :classifier "natives-windows"]
                 [commons-io/commons-io "2.5"]
                 [overtone/at-at "1.2.0"]
                 [kelsey-sorrels/tinter "0.1.1-SNAPSHOT"]
                 [nio "1.0.3"]
                 [org.joml/joml "1.7.1"]
                 [im.bci/pngdecoder "0.13"]
                 [com.taoensso/timbre "4.2.1"]
                 [clojure-watch "0.1.11"]]
  :lein-release {:deploy-via :lein-deploy
                 :scm :git
                 :build-uberjar true}
  :jvm-opts ~(if (-> (System/getProperty "os.name")
                    (.toLowerCase)
                    (.contains "mac"))
              ["-XstartOnFirstThread"
               "-Dorg.lwjgl.opengl.Display.enableHighDPI=true"
               "-Djava.library.path=native/"]
              ["-Djava.library.path=native/"]))
