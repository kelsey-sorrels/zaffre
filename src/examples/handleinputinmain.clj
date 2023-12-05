(ns examples.handleinputinmain
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
           (zaffre.font CP437Font TTFFont)))

(defn hsv->rgb [h s v]
  (let [c (* v s)
        x (* c (- 1.0 (Math/abs (double (dec (mod (/ h 60.0) 2.0))))))]
    (mapv (comp int (partial * 255))
      (cond
        (< h  60) [c x 0]
        (< h 120) [x c 0]
        (< h 180) [0 c x]
        (< h 240) [0 x c]
        (< h 300) [x 0 c]
        (< h 360) [c 0 x]))))

(defn -main [& _]
  ;; render in background thread
   (let [terminal   (zgl/create-terminal [:text :rainbow]
                                       {:title "Zaffre demo"
                                        :columns 80 :rows 24
                                        :default-fg-color [250 250 250]
                                        :default-bg-color [5 5 8]
                                        :windows-font (TTFFont. "Consolas" 12)
                                        :else-font (TTFFont. "Monospaced" 12)
                                        :icon-paths ["images/icon-16x16.png"
                                                     "images/icon-32x32.png"
                                                     "images/icon-128x128.png"]})
        last-key    (atom nil)
        ;; Every 10ms, set the "Rainbow" text to have a random fg color
        fx-chan     (go-loop []
                      (dosync
                        (doseq [x (range (count "Rainbow"))
                                :let [rgb (hsv->rgb (double (rand 360)) 1.0 1.0)]]
                            (zat/set-fx-fg! terminal :rainbow (inc x) 1 rgb)))
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
          \q (zat/destroy! terminal)
          nil)
        (if (= new-key :exit)
          (do
            (async/close! fx-chan)
            (async/close! render-chan)
            (System/exit 0))
          (recur))))))


