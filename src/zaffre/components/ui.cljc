(ns zaffre.components.ui
  (:require
    [overtone.at-at :as atat]
    [taoensso.timbre :as log]
    [zaffre.components :as zc]))

(log/set-level! :info)

(declare Input)
(declare InputSelect) 

(defn input-element-seq [dom]
  (zc/filter-element-tree (fn [{:keys [type]}]
                            (= type Input))
                          dom))

(defn input-select-element [dom]
  (first
    (zc/filter-element-tree (fn [{:keys [type]}]
							  (= type InputSelect))
							dom)))

(defn to-input-char [event]
  (if (= event :space)
    \ 
    event))

(def InputSelect (zc/create-react-class {
  :display-name "InputSelect"
  :get-initial-state (fn [] {:index 0})
  :get-default-props (fn input-get-default-props [] {
    :on-keypress (fn input-select-on-keypress [this e]
                   (log/info "InputSelect on-keypress" (get e :key))
                   (let [{:keys [key dom]} e
                         {:keys [index]} (zc/state this)
                         input-elements (input-element-seq dom)
                         curr-input (nth input-elements (mod index (count input-elements)))]
                     (if (= key :tab)
                       (let [next-input (nth input-elements (mod (inc index) (count input-elements)))]
                         (when-not (= curr-input next-input)
                           ;; blur curr input
                           (let [instance (zc/construct-instance curr-input)
                                 {:keys [on-blur]} (zc/props instance)]
                             (binding [zc/*current-owner* curr-input]
                               (on-blur
                                 (assoc instance :updater zc/*updater*)
                                 {})))
                           ;; focus next input
                           (let [instance (zc/construct-instance next-input)
                                 {:keys [on-focus]} (zc/props instance)]
                             (binding [zc/*current-owner* next-input]
                               (on-focus
                                 (assoc instance :updater zc/*updater*)
                                 {})))
                           ;; update this state
                           (zc/set-state! this (fn [{:keys [index]}]
                                                 {:index (inc index)}))))
                        ;; dispatch event to curr input
                        (let [instance (zc/construct-instance curr-input)
                              {:keys [on-keypress]} (zc/props instance)]
                          (binding [zc/*current-owner* curr-input]
							(on-keypress
							  (assoc instance :updater zc/*updater*)
							  {:key key}))))))})
  :render
    (fn [this]
      (let [{:keys [children] :as props} (zc/props this)]
        (log/info "InputSelect render")
        (first children)))}))
  
(def Input (zc/create-react-class {
    :display-name "Input"
    :get-initial-state (fn []
                         (log/info "Input get-initial-state")
                         {:value ""
                          :show-cursor false
                          :focused false})
    :component-will-mount (fn [this]
                            (log/info "Input component-will-mount" (get zc/*current-owner* :id))
                            ;; pass current-owner binding through to the scheduled fn
                            (let [owner zc/*current-owner*
                                  updater zc/*updater*
                                  cursor-fn (atat/every 400
                                                        #(try
                                                          (binding [zc/*current-owner* owner
                                                                    zc/*updater* updater]
                                                            (log/info "Input component-will-mount cursor-fn" (get owner :id))
                                                            (zc/set-state! this (fn [{:keys [show-cursor]}]
                                                                                  (log/info "Input component-will-mount cursor set-state-fn" (get owner :id) (not show-cursor))
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
              :height 1
              :cursor-char-on \u2592
              :cursor-char-off \space
              :cursor-fg [255 255 255]
              :cursor-bg [0 0 0]}
      :on-focus (fn [this e]
                  (zc/set-state! this (fn [s] (merge s {:focused true}))))
      :on-blur (fn [this e]
                  (zc/set-state! this (fn [s] (merge s {:focused false}))))
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
        (let [{:keys [value show-cursor focused]} (zc/state this)
              {:keys [style] :as props} (zc/props this)
              {:keys [cursor-char-on cursor-char-off
                      cursor-fg cursor-bg]}  style
              cursor (if (and focused show-cursor) cursor-char-on cursor-char-off)]
          (log/info "Input render" show-cursor (dissoc props :children))
          (zc/csx [:view {:style {:border-style :single
                                  :border-bottom 1}} [
                    [:text {} [
                      [:text {} [value]]
                      [:text {:style {:fg cursor-fg :bg cursor-bg}} [(str cursor)]]]]]])))}))
