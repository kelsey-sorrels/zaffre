(ns zaffre.components.events
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]
            [zaffre.components.render :as zcr]
            [zaffre.components :as zc]
            [zaffre.components.ui :as zcui]))

(def active-element (atom nil))
(def tab-index (atom nil))

(defn tabbable-elements
  [container]
  (->> container
    deref
    first
    (filter (fn [[_ props _]] (contains? (get props :class) :tabbable)))
    vec))

(defn advance-tab-index!
  [container]
  (swap! tab-index (fn [index]
    (mod (inc index) (count (tabbable-elements container))))))

(defn decrease-tab-index!
  [container]
  (swap! tab-index (fn [index]
    (mod (dec index) (count (tabbable-elements container))))))

(defn active-element
  [container]
  (when (pos? @tab-index)
    (-> container
      tabbable-elements
      (nth @tab-index))))

(defn send-event-to-dom [event container]
  (log/info "send-events-to-dom" event)
  (if (= event :tab)
    (let [elements (tabbable-elements)
          cur-element (active-element container)
          _ (advance-tab-index! container)
          next-element (active-element container)]
      (when-not (= cur-element next-element)
        ;; blur curr input
        (let [{:keys [on-blur]} (second cur-element)]
          (when on-blur
            (on-blur
              cur-element
              {})))
        ;; focus next input
        (let [{:keys [on-focus]} (second next-element)]
          (when on-focus
            (on-focus
              next-element
              {})))))
        ;; update this state
     ;; dispatch event to curr input
      (let [element (active-element container)
            {:keys [on-keypress]} (second element)]
       (when on-keypress
         (on-keypress
           element
           {:key key})))))

