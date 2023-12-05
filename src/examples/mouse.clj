(ns examples.mouse
  (:require [zaffre.aterminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.aterminal ATerminal)
           (zaffre.font CP437Font)))


(def font (CP437Font. "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green 2 true))
                                     
(defn -main [& _]
  ;; render in background thread
   (let [terminal   (zgl/make-terminal [:text]
                                       {:title "Zaffre demo"
                                        :columns 16 :rows 16
                                        :default-fg-color [250 250 250]
                                        :default-bg-color [5 5 8]
                                        :windows-font font
                                        :else-font font
                                        :icon-paths ["images/icon-16x16.png"
                                                     "images/icon-32x32.png"
                                                     "images/icon-128x128.png"]})
       event-chan   (async/merge
                      [(zat/get-key-chan terminal)
                       (zat/get-mouse-chan terminal)]
                      10)
        last-key    (atom nil)
        trail-length 6
        last-mouse-pos (atom [])
        ;; Every 33ms, draw a full frame
        render-chan (go-loop []
                      (dosync
                        (let [key-in (or @last-key \?)]
                          (zat/clear! terminal)
                          (zutil/put-string terminal :text 0 0 "Hello world")
                          (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
                            (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
                          (zutil/put-string terminal :text 12 0 (str key-in))
                          (doseq [[col row] @last-mouse-pos]
                            (zat/put-chars! terminal :text [{:c \* :fg [200 200 100] :bg [4 4 5] :x col :y row}]))
                          (zat/refresh! terminal)))
                          ;; ~30fps
                        (Thread/sleep 33)
                        (recur))]
    ;; get key presses in fg thread
    (loop []
      (let [new-event (async/<!! event-chan)]
        (cond
          (map? new-event)
            (let [{:keys [mouse-leave]} new-event]
              (log/info "Got mouse-event" new-event (vec @last-mouse-pos))
              (when mouse-leave
                (swap! last-mouse-pos (fn [col] (take trail-length (cons mouse-leave col)))))
              (recur))
          (char? new-event)
            (let [new-key new-event]
              (reset! last-key new-key)
              (log/info "got key" (or (str @last-key) "nil"))
              (case new-key
                \q (zat/destroy! terminal)
                nil)
              (if (= new-key :exit)
                (do
                  (async/close! render-chan)
                  (async/close! event-chan)
                  (System/exit 0))
                ;; change font size on s/m/l keypress
                (recur))))))))


