(ns zaffre.components.ui
  (:require
    [overtone.at-at :as atat]
    [taoensso.timbre :as log]
    [zaffre.components :as zc]))

(log/set-level! :info)

(defn to-input-char [event]
  (if (= event :space)
    \ 
    event))

(def Input (zc/create-react-class {
    :display-name "Input"
    :get-initial-state (fn []
                         (log/info "Input get-initial-state")
                         {:value "" :show-cursor false})
    :component-will-mount (fn [this]
                            (log/info "Input component-will-mount" (get zc/*current-owner* :id))
                            ;; pass current-owner binding through to the scheduled fn
                            (let [owner zc/*current-owner*
                                  updater zc/*updater*
                                  cursor-fn (atat/every 400
                                                        #(try
                                                          (binding [zc/*current-owner* owner
                                                                    zc/*updater* updater]
                                                            (log/info "component-will-mount cursor-fn" (get owner :id))
                                                            (zc/set-state! this (fn [{:keys [show-cursor]}]
                                                                                  (log/info "component-will-mount cursor set-state-fn" (get owner :id) (not show-cursor))
                                                                                  {:show-cursor (not show-cursor)})))
                                                          (catch Exception e
                                                            (log/error e)))
                                                        zc/*pool*)]
                              (zc/set-state! this {:cursor-fn cursor-fn})))
    :component-will-unmount (fn [this]
                              (let [{:keys [cursor-fn]} (zc/state [this])]
                                (log/info "Input unmounting" cursor-fn)
                                (atat/stop cursor-fn)))
    :get-default-props (fn input-get-default-props [] {
      :max-length 28
      :style {:width 30
              :height 1}
      :on-focus (fn [this e] nil)
      :on-blur (fn [this e] nil)
      :on-keypress (fn input-on-keypress [this e]
                     (log/info "Input on-keypress" e)
                     (let [{:keys [max-length]} (zc/props this)
                           k (get e :key)]
                       (cond
                         (= k :backspace)
                             (zc/set-state! this (fn [{:keys [value]}]
                                                   {:value (subs value 0 (dec (count value)))}))
                         (and (or (char? k) (= k :space)) (not= k \newline))
                           (zc/set-state! this (fn [{:keys [value]}]
                                                 (if (< (count value) max-length)
                                                   {:value (str value (to-input-char k))}
                                                   {:value value}))))))})
    :render
      (fn [this]
        (let [{:keys [value show-cursor]} (zc/state this)
              cursor (when show-cursor \u2592)]
          (log/info "Input render" show-cursor)
          (zc/csx [:view {:style {:border-style :single
                                  :border 1}} [
                    [:text {} [(str value cursor)]]]])))}))
