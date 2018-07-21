(ns zaffre.components.ui
  (:require
    [taoensso.timbre :as log]
    [zaffre.components :as zc]))

(def Input (zc/create-react-class {
    :display-name "Input"
    :get-initial-state (fn []
                         (log/info "Input get-initial-state")
                         {:value "value"})
    :get-default-props (fn input-get-default-props [] {
      :on-focus (fn [this e] nil)
      :on-blur (fn [this e] nil)
      :on-keypress (fn input-on-keypress [this e]
                     (log/info "Input on-keypress" e)
                     (let [k (get e :key)]
                       (if (= k \backspace)
                         (zc/set-state this (fn [{:keys [value]}]
                                              {:value (subs value 0 (dec (count value)))}))
                         (zc/set-state this (fn [{:keys [value]}]
                                              {:value (str value k)})))))})
    :render
      (fn [this]
        (let [{:keys [value]} (zc/state this)]
          (zc/csx [:view {:style {:border-style :single :border-bottom 1}} [
                    [:text {} [(str value)]]]])))}))
