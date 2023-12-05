(ns examples.mouse
  (:require [zaffre.terminal :as zat]
            [zaffre.glterminal :as zgl]
            [zaffre.events :as zevents]
            [zaffre.font :as zfont]
            [zaffre.util :as zutil]
            [clojure.core.async :as async :refer [go-loop]]
            [taoensso.timbre :as log])
  (:import (zaffre.terminal Terminal)
           (zaffre.font CP437Font)))


(def font (zfont/scale (zfont/cp-437 "http://dwarffortresswiki.org/images/b/be/Pastiche_8x8.png" :green true) 2))
                                     
(defn -main [& _]
  ;; render in background thread
   (zgl/create-terminal
     [{:id :app
        :layers [:text]
        :columns 16
        :rows 16
        :pos [0 0]
        :font (constantly font)}]
     {:title "Zaffre demo"
      :screen-width (* 16 16)
      :screen-height (* 16 16)
      :default-fg-color [250 250 250]
      :default-bg-color [5 5 8]}
     (fn [terminal]
       (let [last-key    (atom nil)
             trail-length 6
             last-mouse-pos (atom [])]
             ;; Every 33ms, draw a full frame
         (zat/do-frame terminal 33
           (let [key-in (or @last-key \?)]
             (zutil/put-string terminal :text 0 0 "Hello world")
             (doseq [[i c] (take 23 (map-indexed (fn [i c] [i (char c)]) (range (int \a) (int \z))))]
               (zutil/put-string terminal :text 0 (inc i) (str c) [128 (* 10 i) 0] [0 0 50]))
             (zutil/put-string terminal :text 12 0 (str key-in))
             (doseq [[col row] @last-mouse-pos]
               (zat/put-chars! terminal :text [{:c \* :fg [200 200 100] :bg [4 4 5] :x col :y row}]))))
         ;; get key presses in fg thread
         (zevents/add-event-listener terminal :keypress
           (fn [new-key]
             (reset! last-key new-key)
             (log/info "got key" (or (str @last-key) "nil"))
             (case new-key
               \q (zat/destroy! terminal)
               nil)))
         (zevents/add-event-listener terminal :mouse-leave
           (fn [{:keys [col row]}]
             ;(log/info "got mouse-leave" [col row])
             (swap! last-mouse-pos (fn [coll] (take trail-length (cons [col row] coll))))))))))


