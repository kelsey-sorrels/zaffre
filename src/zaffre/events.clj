(ns zaffre.events
  (:require [zaffre.terminal :as zat]
            [clojure.core.async :as async :refer [go go-loop]]))

(defn add-event-listener [terminal event-type f]
  (let [event-chan    (async/chan)
        listener-chan (go-loop []
                        (when-not (zat/destroyed? terminal)
                          (let [ev (async/<! event-chan)]
                            (f (first ev))
                            (recur))))]
    (async/sub (zat/pub terminal) event-type event-chan)
    event-chan))

(defn wait-for-close
  ([terminal]
   (wait-for-close terminal []))
  ([terminal chans]
  (let [close-chan (async/chan)]
    (async/sub (zat/pub terminal) :close close-chan)
    (async/<! close-chan)
    (doseq [chan chans]
      (async/close! chan)))))
