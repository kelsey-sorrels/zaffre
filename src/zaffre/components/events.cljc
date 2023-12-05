(ns zaffre.components.events
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]
            [zaffre.terminal :as zt]))

(defn send-event-to-dom [event dom]
  (log/info "send-events-to-dom" event)
  ;; Find InputSelect element
  (when-let [input-select-elem (zcui/input-select-element dom)]
    (binding [zc/*current-owner* input-select-elem]
      (let [instance (zc/construct-instance input-select-elem)
            {:keys [on-keypress]} (zc/props instance)]
        (log/info "send-event-to-dom on-keypress" on-keypress)
        ;; Send key event to InputSelect instance
        (on-keypress
          (assoc instance :updater zc/*updater*)
          {:key event :dom dom})))))

(defn send-events-to-dom [events dom]
  (doseq [event events]
    (send-event-to-dom event dom))
  ;; Take all enqueued state changes and reduce them down to new state
  (zc/update-state! zc/*updater*))

