(ns zaffre.components.ui
  (:require
    [zaffre.components :as zc]))

(def Input (zc/create-react-class {
    :display-name "Input"
    :get-initial-state (fn [] {:value ""})
    :on-focus (fn [this e] nil)
    :on-blur (fn [this e] nil)
    :on-keypress (fn [this e]
                   (let [key-code (get e :key-code)]
                     (if (= (get e :key-code) \backspace)
                       (zc/set-state this (fn [{:keys [value]}]
                                            {:value (subs value 0 (dec (count value)))}))
                       (zc/set-state this (fn [{:keys [value]}] {:value (str value key-code)})))))
    :on-change (fn [this e] nil)
    :render
      (fn [this]
        (let [{:keys [value]} (zc/props this)]
          (zc/csx [:view {:style {:border-style :single :border-bottom 1}} [
                    [:text {} [value]]]])))}))
