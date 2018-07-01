(ns zaffre.components.input
  (:require
    [zaffre.components :as zc]))

(def Input (zc/create-react-class
    :display-name "Input"
    :derive-state-from-props [this]
    :on-focus (fn [this e] nil)
    :on-blur (fn [this e] nil)
    :on-keypress (fn [this e] nil)
    :render
      (fn [this]
        (let [{:keys [value]} (zc/props this)]
          (zc/csx [:view {:style {:border-bottom :single}} [
                    (zc/csx [:text {} value])]])))))
