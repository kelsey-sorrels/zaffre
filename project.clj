(defproject zaffre "0.4.0-SNAPSHOT"
  :description "A fast Clojure console library"
  :url "https://github.com/kelsey-sorrels/zaffre"
  :license {:name "The MIT License (MIT)"
            :url "https://raw.githubusercontent.com/kelsey-sorrels/zaffre/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.cache "0.7.1"]
                 [org.clojure/core.async "0.3.465"]
                 ^{:voom {:repo "https://github.com/rogerallen/hello_lwjgl" :branch "master"}}
                 [hello_lwjgl "0.4.0-20180105_044833-g683b3f6"]
                 [commons-io/commons-io "2.5"]
                 [overtone/at-at "1.2.0"]
                 [kelsey-sorrels/tinter "0.1.1-SNAPSHOT"]
                 [rockpick "0.1.0-SNAPSHOT"]
                 [nio "1.0.3"]
                 [clj-http "3.9.0"]
                 [org.joml/joml "1.7.1"]
                 [im.bci/pngdecoder "0.13"]
                 [com.taoensso/timbre "4.2.1"]
                 [clojure-watch "0.1.11"]]
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
