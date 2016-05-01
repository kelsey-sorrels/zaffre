(ns zaffre.imageutil
  (:require [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (java.awt.image BufferedImage DataBufferByte)
           (java.nio ByteBuffer)
           (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFWImage GLFWImage$Buffer)
           (de.matthiasmann.twl.utils PNGDecoder PNGDecoder$Format)))

(defn buffered-image-byte-buffer [^BufferedImage buffered-image]
  (let [width          (.getWidth buffered-image)
        height         (.getHeight buffered-image)
        texture-buffer (BufferUtils/createByteBuffer (* width height 4))
        data ^bytes (-> buffered-image
                 (.getRaster)
                 (.getDataBuffer)
                 (as-> db
                   (let [dbb ^DataBufferByte db]
                     (.getData dbb))))]
    (doseq [i (range (quot (alength data) 4))]
      (doseq [n (reverse (range 4))]
        (.put texture-buffer (aget data (+ (* i 4) n)))))
    (.flip texture-buffer)
    texture-buffer))


(defn png-bytes [path]
  (with-open [input-stream (jio/input-stream path)]
    (let [decoder (PNGDecoder. input-stream)
          width (.getWidth decoder)
          height (.getHeight decoder)
          bytebuf (ByteBuffer/allocateDirect (* width height 4))]
      (.decode decoder bytebuf (* width 4) PNGDecoder$Format/RGBA)
      (.flip bytebuf)
      bytebuf)))

(defn icons [paths]
  (let [icons (GLFWImage/create (count paths))]
    (loop [idx 0 icon-bytebuffers (mapv png-bytes paths)]
      (if-let [^ByteBuffer icon-bytebuffer (first icon-bytebuffers)]
        (let [width (int (Math/sqrt (quot (.limit icon-bytebuffer) 4)))
              height width]
          (log/info "Icon" idx "(" width "x" height ")" (.limit icon-bytebuffer))
          (doto icons
            (.position idx)
            (.width width)
            (.height height)
            (.pixels icon-bytebuffer))
          (recur (inc idx) (next icon-bytebuffers)))
         (do
           (.flip icons)
           icons))))) 
