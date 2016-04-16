(defproject zaffre "0.4.0-SNAPSHOT"
  :description "A fast Clojure console library"
  :url "https://github.com/aaron-santos/zaffre"
  :license {:name "The MIT License (MIT)"
            :url "https://raw.githubusercontent.com/aaron-santos/zaffre/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [hello_lwjgl/lwjgl "3.0.0b1"]
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
              ["-XstartOnFirstThread" "-Dorg.lwjgl.opengl.Display.enableHighDPI=true"]
              []))
