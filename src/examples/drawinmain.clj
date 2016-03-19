(ns examples.drawinmain
  (:require [zaffre.aterminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.aterminal ATerminal)
           (zaffre.font CP437Font TTFFont)))

(defn -main [& _]
  ;; render in background thread
  (let [colorShift    (atom 0.0001)
        brightness    (atom 0.68)
        contrast      (atom 2.46)
        scanlineDepth (atom 0.94)
        time          (atom 0.0)
        noise         (atom 0.0016)
        terminal      (zgl/make-terminal [:text :rainbow]
                                         {:title "Zaffre demo"
                                          :columns 80 :rows 24
                                          :default-fg-color [250 250 250]
                                          :default-bg-color [5 5 8]
                                          :windows-font (CP437Font. "http://dwarffortresswiki.org/images/2/29/Potash_8x8.png" :green 1)
                                          :else-font (CP437Font. "http://dwarffortresswiki.org/images/2/29/Potash_8x8.png" :green 1)
                                          :antialias true
                                          :icon-paths ["images/icon-16x16.png"
                                                       "images/icon-32x32.png"
                                                       "images/icon-128x128.png"]
                                          :fx-shader {:name     "retro.fs"
                                                      :uniforms [["time" @time]
                                                                 ["noise" @noise]
                                                                 ["colorShift" @colorShift]
                                                                 ["scanlineDepth" @scanlineDepth]
                                                                 ["brightness" @brightness]
                                                                 ["contrast" @contrast]]}})
        last-key   (atom nil)
        input-chan (go-loop []
          (reset! last-key (async/<!! (zat/get-key-chan terminal)))
          (log/info "got key" (or (str @last-key) "nil"))
          ;; change font size on s/m/l keypress
          (case @last-key
            \s (zat/apply-font! terminal
                 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1)
                 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 1))
            \m (zat/apply-font! terminal
                 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2)
                 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2))
            \l (zat/apply-font! terminal
                 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 3)
                 (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 3))
            nil)
          (recur))]
    ;; get key presses in fg thread
    (loop []
      (let [key-in (or @last-key \?)]
        (dosync
          (zat/clear! terminal)
          (zutil/put-string terminal :text 0 0 "Hello world")
          (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
            (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
          (zutil/put-string terminal :text 12 0 (str key-in))
          (zat/refresh! terminal))
        (if (= key-in :exit)
          (do
            (async/close! input-chan)
            (log/info "Got :exit. Stopping")
            (System/exit 0))
          (recur))))))

