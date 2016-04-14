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
   (zgl/make-terminal
     [:text]
     {:title "Zaffre demo"
      :columns 16 :rows 16
      :default-fg-color [250 250 250]
      :default-bg-color [5 5 8]
      :windows-font font
      :else-font font
      :icon-paths ["images/icon-16x16.png"
                   "images/icon-32x32.png"
                   "images/icon-128x128.png"]}
     (fn [terminal]
       (let [term-pub    (zat/pub terminal)
             key-chan    (async/chan)
             mouse-leave-chan (async/chan)
             close-chan  (async/chan)
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
         (async/sub term-pub :keypress key-chan)
         (async/sub term-pub :mouse-leave mouse-leave-chan)
         (async/sub term-pub :close close-chan)
         ;; get key presses in fg thread
         (go-loop []
           (let [new-key (async/<! key-chan)]
             (reset! last-key new-key)
             (log/info "got key" (or (str @last-key) "nil"))
             (case new-key
               \q (zat/destroy! terminal)
               nil)))
         (go-loop []
           (let [{:keys [col row]} (async/<! mouse-leave-chan)]
             (log/info "got mouse-leave" [col row])
             (swap! last-mouse-pos (fn [coll] (take trail-length (cons [col row] coll)))))
             (recur))
         (let [_ (async/<! close-chan)]
           (async/close! render-chan)
           (async/close! key-chan)
           (async/close! mouse-leave-chan)
           (System/exit 0))))))


