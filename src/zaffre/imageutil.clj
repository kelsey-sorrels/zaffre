(ns zaffre.imageutil
  (:require [clojure.java.io :as jio]
            [taoensso.timbre :as log])
  (:import (java.awt.image BufferedImage DataBufferByte)
           (java.nio ByteBuffer)
           (org.apache.commons.io IOUtils)
           (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFWImage GLFWImage$Buffer)
           (org.lwjgl.stb STBImage STBImageResize STBImageWrite)
           (de.matthiasmann.twl.utils PNGDecoder PNGDecoder$Format)))

(defprotocol Dimensions
  (width [this])
  (height [this]))

(defrecord Image [width height channels byte-buffer]
  Dimensions
  (width [_] width)
  (height [_] height))

(defn image
  ([width height]
    (image width height 4))
  ([width height channels]
    (log/info "Creating buffer" width height channels)
    (->Image width height channels (BufferUtils/createByteBuffer (* width height channels)))))

(defn load-image [location]
  (let [w           (BufferUtils/createIntBuffer 1)
        h           (BufferUtils/createIntBuffer 1)
        c           (BufferUtils/createIntBuffer 1)
        buffer      (->
                      location
                      jio/input-stream
                      IOUtils/toByteArray
                      ByteBuffer/wrap)
        byte-buffer (STBImage/stbi_load_from_memory buffer w h c 0)]
    (->Image (.get w) (.get h) (.get c) byte-buffer)))

(defn write-png [{:keys [width height channels byte-buffer] :as img} path]
  (STBImageWrite/stbi_write_png path width height channels byte-buffer 0)
  img)

(defn scale [{:keys [width height channels byte-buffer]} ^long s]
  (let [x            (BufferUtils/createIntBuffer 1)
        y            (BufferUtils/createIntBuffer 1)
        c            (BufferUtils/createIntBuffer 1)
        scaled-bytes (BufferUtils/createByteBuffer (* (* s width)
                                                      (* s height)
                                                      channels))]
    (when (zero?
            (STBImageResize/stbir_resize_uint8
              byte-buffer width height 0
              scaled-bytes (* s width) (* s height) 0
              channels))
      (throw (RuntimeException. "Error scaling image")))
    (->Image (* s width) (* s height) channels scaled-bytes)))

(defn copy-sub-image [{dwidth :width dheight :height dchannels :channels dbytes :byte-buffer}
                      {swidth :width sheight :height schannels :channels sbytes :byte-buffer}
                      dx1 dy1 sx1 sy1 sx2 sy2]
  {:pre [(= dchannels schannels)]}
  (doseq [x (range (- sx2 sx1))
          y (range (- sy2 sy1))
          :let [sidx (+ sx1 (* (+ y sy1) swidth schannels))
                didx (+ dx1 (* (+ x dy1) dwidth dchannels))]]
      (.position dbytes didx)
      (.position sbytes sidx)
      (doseq [_ (range dchannels)]
        (.put dbytes (.get sbytes))))
  (.flip sbytes))

(defn draw-image [dest-img
                  {:keys [width height] :as src-img}
                  x y]
  (copy-sub-image dest-img src-img x y 0 0 width height))
                      

(defn- copy-channel [v channel]
  (let [channel-shift (case channel
                       :alpha 24
                       :blue  16
                       :green  8
                       :red    0)
        c (->
            (bit-shift-left 0xFF channel-shift)
            (bit-and v)
            (unsigned-bit-shift-right channel-shift))]
    (reduce (fn [a n] (bit-or a (bit-shift-left c n))) 0 [24 16 8 0])))

(defn resize [{swidth :width sheight :height :as img} width height]
  (copy-sub-image (image width height)
                  img
                  0 0
                  0 0
                  swidth sheight))
                  
(defn copy-channels [{:keys [width height channels byte-buffer]} channel]
  (let [img           (image width height)
        result-buffer (BufferUtils/createByteBuffer (* width height))]
    (doseq [x (range width)
            y (range height)]
      (.putLong result-buffer
                (copy-channel
                  (.getLong byte-buffer)
                  channel)))
    (.flip result-buffer)
    (->Image width height channels result-buffer)))

