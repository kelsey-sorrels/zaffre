(ns zaffre.components
  (:require clojure.data
            clojure.string
            [taoensso.timbre :as log]))
 
(declare ^:dynamic *current-owner*)

;; State
(defprotocol IUpdater
  (enqueue-set-state! [this element partial-state callback])
  (update-state! [this])
  (element-state-changed? [this element])
  (get-state [this element])
  (get-prev-state [this element]))

;; Updater consists of q: a queue of update fns, and s: a map from key to state.
(defrecord Updater [a]
  Object
  (toString [this]
    (let [updater @a]
      (format "Updater\npending:%d\nstate:\n%s\nprev-state:\n%s"
        (-> updater :q count)
        (clojure.string/join "\n"
          (map (fn [[k v]]
                 (format "%x: %s"(System/identityHashCode k) v))
               (get updater :s)))
        (clojure.string/join "\n"
          (map (fn [[k v]]
                 (format "%x: %s"(System/identityHashCode k) v))
               (get updater :prev-state))))))
  IUpdater
  (enqueue-set-state! [this element partial-state callback]
    (assert (some? element) "Element must not be nil")
    (assert (-> element :id some?) "Element must have :id")
    (let [k (get element :id)
          callback (cond
                     (map? partial-state)
                       (fn [s]
                         (update-in s [k] #(merge % partial-state)))
                     (fn? callback)
                       (fn [s]
                         (update s k callback))
                     :else
                       (fn [s] nil))]
      (swap! a (fn [{:keys [q prev-state] :as updater}]
                 (assoc updater :q (conj q callback)))))
      this)
  (update-state! [this]
    (swap! a (fn [{:keys [q s] :as updater}]
               ;; apply queued changes to old state to make new state
               (let [new-s (reduce (fn [s f] (f s)) s q)
                     ;; diff the old state and new state to find changed element ids
                     [a b _] (clojure.data/diff s new-s)
                     dirty (clojure.set/union (keys a) (keys b))]
                 (assoc updater :q []
                                :s new-s
                                :prev-state s))))
    this)
  (element-state-changed? [this element]
    (assert (some? element) "Element must not be nil")
    (assert (-> element :id some?) "Element must have :id")
    (let [k (get element :id)
          {:keys [s prev-state] :as updater} @a]
      (= (get s k) (get prev-state k))))
  (get-state [this element]
    (let [k (get element :id)]
      (-> a deref :s (get k))))
  (get-prev-state [this element]
    (let [k (get element :id)]
      (-> a deref :prev-state (get k)))))

(defn empty-state [] (->Updater (atom {:q [] :s {} :prev-state {}})))

(defrecord NoopUpdater []
  IUpdater
  (enqueue-set-state! [this element partial-state callback]
    (assert (-> element :id some?) "Element must not be nil")
      this)
  (update-state! [this]
    this)
  (element-state-changed? [this element]
    (assert (-> element :id some?) "Element must not be nil")
    false)
  (get-state [this element] nil)
  (get-prev-state [this element] nil))

(def noop-updater (->NoopUpdater))

(def ^:dynamic *updater* noop-updater)

;; PureComponent
;; context is for context api
;; updater schedules state changes
(defprotocol LifecycleMethods
  ;; Mounting
  (component-will-mount [this])
  (render [this])
  (component-did-mount [this])
  ;; Updating
  (component-will-receive-props [this next-props])
  (should-component-update? [this next-props next-state])
  (component-will-update [this next-props next-state])
  (get-snapshot-before-update [this])
  (component-did-update [this prev-props prev-state])
  (get-derived-state-from-props [this next-props prev-state])
  ;; Unmounting
  (component-will-unmount [this])
  ;; Error Handling
  (component-did-catch [this]))

(defprotocol ComponentMethods
  (set-state [this update])
  (force-update [this callback]))

(defprotocol ComponentProperties
  (default-props [this])
  (display-name [this]))

(defprotocol ConstructorMethods
  (create-instance [this] [this state]))

(defprotocol InstanceProperties
  (props [this])
  (state [this]))


(declare ->ComponentInstance)
(defrecord Component [updater
                      component-will-mount
                      render
                      component-did-mount
                      component-will-receive-props
                      should-component-update?
                      component-will-update
                      get-snapshot-before-update
                      component-did-update
                      component-will-unmount
                      component-did-catch
                      default-props
                      get-initial-state
                      get-derived-state-from-props
                      display-name
                      props]
  Object
  (toString [this]
    (format "Component %s :props %s"
      (get this :display-name)
      (get this :props)))
  LifecycleMethods
  ;; Mounting
  (component-will-mount [this]
    ((get this :component-will-mount) this))
  (render [this]
    ((get this :render) this))
  (component-did-mount [this]
    ((get this :component-did-mount) this))
  ;; Updating
  (component-will-receive-props [this next-props]
    ((get this :component-will-receive-props) this next-props))
  (should-component-update? [this next-props next-state]
    ((get this :should-component-update?) this next-props next-state))
  (component-will-update [this next-props next-state]
    ((get this :component-will-update) this next-props next-state))
  (get-snapshot-before-update [this]
    (get-snapshot-before-update this))
  (component-did-update [this prev-props prev-state]
    ((get this :component-did-update) this prev-props prev-state))
  (get-derived-state-from-props [this next-props prev-state]
    ((get this :get-derived-state-from-props) this next-props prev-state))
  ;; Unmounting
  (component-will-unmount [this]
    ((get this :component-will-unmount) this))
  ;; Error Handling
  (component-did-catch [this]
    ((get this :component-did-catch) this))

  ComponentProperties
  (default-props [this] (get this :default-props))
  (display-name [this] (get this :display-name))

  ConstructorMethods
  (create-instance [this]
    (->ComponentInstance this props (get-initial-state)))
  (create-instance [this state]
    (->ComponentInstance this props state)))

(defn component? [type]
  (instance? Component type))

(defrecord ComponentInstance [component props state]
  Object
  (toString [this]
    (format "Component %s :props %s :state %s"
      (get this :display-name)
      (get this :props)
      (get this :state)))
  LifecycleMethods
  ;; Mounting
  (component-will-mount [this]
    ((get component :component-will-mount) this))
  (render [this]
    ((get component :render) this))
  (component-did-mount [this]
    ((get component :component-did-mount) this))
  ;; Updating
  (component-will-receive-props [this next-props]
    ((get component :component-will-receive-props) this next-props))
  (should-component-update? [this next-props next-state]
    ((get component :should-component-update?) this next-props next-state))
  (component-will-update [this next-props next-state]
    ((get component :component-will-update) this next-props next-state))
  (get-snapshot-before-update [this]
    ((get component :get-snapshot-before-update this)))
  (component-did-update [this prev-props prev-state]
    ((get component :component-did-update) this prev-props prev-state))
  (get-derived-state-from-props [this next-props prev-state]
    ((get component :get-derived-state-from-props) this next-props prev-state))
  ;; Unmounting
  (component-will-unmount [this]
    ((get component :component-will-unmount) this))
  ;; Error Handling
  (component-did-catch [this]
    ((get component :component-did-catch) this))

  ComponentMethods
  (set-state [this update]
    (if (or (map? update) (fn? update))
      (let [updater (get type :updater)
            partial-state (when (map? update) update)
            callback (when (fn? update update))]
        (enqueue-set-state! updater *current-owner* partial-state callback))
      (throw "setState(...): takes an object of state variables to update or a
             function which returns an object of state variables.")))
  (force-update [this callback]
    (let [updater (get type :updater)]
      (enqueue-set-state! updater *current-owner* nil callback)))

  ComponentProperties
  (default-props [this] (default-props component))
  (display-name [this] (display-name component))

  InstanceProperties
  (props [this]
    (get this :props))
  (state [this]
    (get this :state)))
  
(defn component-instance? [instance]
  (instance? ComponentInstance instance))

(def valid-event-handler-keys #{
                                ;; keyboard events
                                :on-keydown
                                :on-keyup
                                :on-keypress
                                ;; focus events
                                :on-focus
                                :on-blur
                                ;; mouse events
                                :on-click
                                :on-double-click
                                :on-drag
                                :on-drag-end
                                :on-drag-enter
                                :on-drag-exit
                                :on-drag-leave
                                :on-drag-over
                                :on-drag-start
                                :on-drag-stop
                                :on-drop
                                :on-mouse-down
                                :on-mouse-enter
                                :on-mouse-leave
                                :on-mouse-move
                                :on-mouse-out
                                :on-mouse-over
                                :on-mouse-up
                                ;; wheel events
                                :on-wheel
                                ;; animation events
                                :on-animation-start
                                :on-animation-end
                                :on-animation-iteration})

(defn is-valid-event-handler-key? [k]
  (contains? valid-event-handler-keys k))

(def default-opts {
  :component-will-mount (fn [this] nil)
  :render (fn [this] nil)
  :component-did-mount (fn [this] nil)
  :component-will-receive-props (fn [this next-props] nil)
  :should-component-update? (fn [this next-props next-state]
                              (or (not= (props this) next-props)
                                  (not= (state this) next-state)))
  :component-will-update (fn [this next-props next-state] nil)
  :get-snapshot-before-update (fn [this] nil)
  :component-did-update (fn [this prev-props prev-state] nil)
  :component-will-unmount (fn [this] nil)
  :component-did-catch (fn [this error info] nil)
  :get-default-props (fn [] {})
  :get-initial-state (fn [] nil)
  :get-derived-state-from-props (fn [this next-props prev-state] prev-state)})

(defn create-react-class
  [opts]
  (assert (every? is-valid-event-handler-key?
                  (filter (fn [k] (clojure.string/starts-with? (name k) "on-"))
                          (keys opts)))
    "Found unsupported event")
  (letfn [(get-opts [k] (get opts k (get default-opts k)))]
    (let [component-will-mount         (get-opts :component-will-mount)
          render                       (get opts :render)
          component-did-mount          (get-opts :component-did-mount)
          component-will-receive-props (get-opts :component-will-receive-props)
          should-component-update?     (get-opts :should-component-update?)
          component-will-update        (get-opts :component-will-update)
          component-did-mount          (get-opts :component-did-mount)
          get-snapshot-before-update   (get-opts :get-snapshot-before-update)
          component-did-update         (get-opts :component-did-update)
          component-will-unmount       (get-opts :component-will-unmount)
          component-did-catch          (get-opts :component-did-catch)
          default-props                ((get-opts :get-default-props))
          get-initial-state            (get-opts :get-initial-state)
          get-derived-state-from-props (get-opts :get-derived-state-from-props)
          display-name                 (get opts :display-name "")]
      (assert render "Class specification must implement a `render` method.")
      (->Component *updater*
                   component-will-mount
                   render
                   component-did-mount
                   component-will-receive-props
                   should-component-update?
                   component-will-update
                   get-snapshot-before-update
                   component-did-update
                   component-will-unmount
                   component-did-catch
                   default-props
                   get-initial-state
                   get-derived-state-from-props
                   display-name
                   props))))

(defn fn->component [render & more]
  (let [display-name (first more)]
    (create-react-class {
      :display-name display-name
      :render
      (fn [this]
        ;; get component and pass to render
        (let [props (props this)]
          (render this)))})))

(defn reserved-prop? [prop-name]
  (contains? #{:key :ref :self :source :state} prop-name))

(defn has-valid-ref? [config] (true? (get config :ref)))
(defn has-valid-key? [config] (true? (get config :key)))

;; ReactElement
(defrecord ReactElement [id type key ref self source owner props]
  Object
  (toString [this]
    (format "Element %s :props %s :state %s"
      (if (keyword? (get this :type))
        (name (get this :type))
        (display-name (get this :type)))
      props
      (get-state *updater* this))))

(defn element-children [element]
  (get-in element [:props :children]))

(defn assoc-children [element children]
  (assoc-in element [:props :children] children))

(defn element-without-children [element]
  (assoc-children element []))

(defn map-children [f element & more]
  "Maps element's childrend and more's children calling (f element-child & more-children).
   Returns updated children."
  ;; `more` may contain nil elements. nil elements return infinite nil children
  (letfn [(element-children-or-nil [element] (or (element-children element) (repeat nil)))]
    (let [elements (cons element more)
          children (map element-children-or-nil elements)
          _ (assert (every? coll? children) (str "Non collection children " (vec (map type children))))
          children (apply map vector children)
          children (vec (take-while (fn [v] (not (nil? v))) children))
          new-children (vec (map (fn [child-vec] (apply f child-vec))children))]
      new-children)))

(defn map-in-children [f element & more]
  "Maps element's childrend and more's children calling (f element-child & more-children).
   Returns element with updated children."
  (assoc-in element [:props :chilren] (apply map-children f element more)))

(defn component? [v]
  (instance? Component v))

(defn element? [v]
  (instance? ReactElement v))

(defn- valid-children? [children]
  (assert (sequential? children) (format "%s not sequential" children))
  (assert (every? (fn [child] (or (component? child)
                                  (element? child)
                                  (string? child)
                                  (nil? child)))
                  children)
          (str " Not every child is a component or element " (vec children)))
  true)

;; React.createElement(Hello, {toWhat: 'World'}, null)
(defn create-element [type config children & more]
  (assert (every? is-valid-event-handler-key?
                  (filter (fn [k] (clojure.string/starts-with? (name k) "on-"))
                          (keys config)))
    "Found unsupported event")
  (let [ref    (if (and config (has-valid-ref? config)) (get config :ref))
        key    (if (and config (has-valid-key? config)) (get config :key))
        self   (get config :self)
        source (get config :source)
        ;; either a single children array arg or multiple children using & more
        children (if (not (empty? more)) (cons children more) children)
        _ (valid-children? children)
        props  (merge {:children children}
                      (if type (get type :default-props {}) {})
                      (into {}
                        (remove (fn [[prop-name _]]
                                  (reserved-prop? prop-name)))
                                config))
        element (->ReactElement nil type key ref self source *current-owner* props)]
    (assoc element :id (System/identityHashCode element))))

(defn create-factory [type]
  (partial create-element type))

;; cloneAndReplaceKey
(defn assoc-key [element key]
  (assoc element :key key))

(defmacro csx [v]
  (let [_ (log/info "expanding" v)
        [type# props# children#] (case (count v)
                                   1 [(first v) {} []]
                                   2 [(first v) (second v) []]
                                   3 [(first v) (second v) (last v)]
                                   [(first v) (second v) (drop 2 v)])]
    `(let [type-value# ~type#
           #_#_display-name# (display-name type-value#)
           ; TODO: is there a better way to calculate display-name?
           config# (merge {#_#_:display-name display-name#
                           #_#_:state (get ~type# :initial-state)}
                          ~props#)]
       (create-element type-value# config# ~children#))))

(defmacro def-component [sym & fn-tail]
  "(def-component red-text [this] (zc/csx [:text (props this) [\"label\"]]))"
  (let [sym# (symbol sym)
        display-name# (str sym#)]
    (list 'def sym# (list create-react-class {
               :display-name display-name#
               :render `(fn ~@fn-tail)}))))

;; Higher Order Components
(defn compose [& funcs]
  (reduce (fn [a b] (fn [& args] (a (apply b args)))) funcs))


(defmacro def-hoc [sym args1 args2 & body]
  (list `defn sym args1
     (apply list `fn args2 `(~@body))))

(def lifecycle-opts #{
  :component-will-mount
  :component-did-mount
  :component-will-receive-props
  :should-component-update?
  :component-will-update
  :get-snapshot-before-update
  :component-did-update
  :component-will-unmount
  :component-did-catch
})

(defn lifecycle-opt? [k]
  (contains? lifecycle-opts k))

(defn lifecycle [spec]
  (fn [base-component]
    (let [non-lifecycle-keys (clojure.set/difference lifecycle-opts (keys spec))]
      (assert (empty? non-lifecycle-keys) (str "Found non-lifecycle key." non-lifecycle-keys)))
    (let [factory (create-factory base-component)]
      (letfn [(render [component]
                      (let [props (props component)
                            state (state component)]
                        (factory {:props props :state state} [])))]
        (let [opts (merge spec {:render render
                                :display-name (str (display-name base-component) "WithLifecycle")})]
          (create-react-class opts))))))

(defn debug-lifecycle-handlers []
  (let [spec {:component-will-mount (fn [this]
                                      (log/info (display-name this) "component-will-mount"))
              :component-did-mount (fn [this]
                                      (log/info (display-name this) "component-did-mount"))
              :component-will-receive-props (fn [this next-props] (log/info (display-name this) "component-will-receive-props"))
              :should-component-update? (fn [this next-props next-state]
                                          (log/info (display-name this) "should-component-update?")
                                          (or (not= (props this) next-props)
                                              (not= (state this) next-state)))
              :component-will-update (fn [this next-props next-state] 
                                       (log/info (display-name this) "component-will-update"))
              :get-snapshot-before-update (fn [this]
                                            (log/info (display-name this) "get-snapshot-before-update"))
              :component-did-update (fn [this prev-props prev-state]
                                      (log/info (display-name this) "component-did-update"))
              :component-will-unmount (fn [this]
                                        (log/info (display-name this) "component-will-unmount"))
              :component-did-catch (fn [this error info]
                                          (log/info (display-name this) "component-did-catch"))}]
   (lifecycle spec))) 

(defn component-display-name [component]
  (cond
    (keyword? component)
      (name component)
    :default
      (display-name component)))

(defn element-display-name [element]
  (component-display-name (get element :type)))

(defmulti tree-level->str (fn [v _] (type v)))
(defmethod tree-level->str nil [s level]
  (let [indent (apply str (repeat (* 2 level) " "))]
    (format "%s%s (Nil @%s)" indent s (System/identityHashCode s))))
(defmethod tree-level->str String [s level]
  (let [indent (apply str (repeat (* 2 level) " "))]
    (format "%s%s (String @%s)" indent s (System/identityHashCode s))))
(defmethod tree-level->str Component [component level]
  (let [indent (apply str (repeat (* 2 level) " "))]
    (apply str
      [(format "%s%s (Component @%x)\n" indent (component-display-name component) (System/identityHashCode component))
       (format "%s  :props %s\n" indent (dissoc (props component) :children))
       (format "%s  :state %s\n" indent (when-let [s (state component)] @s))])))
(defmethod tree-level->str ComponentInstance [instance level]
  (let [indent (apply str (repeat (* 2 level) " "))]
    (apply str
      [(format "%s%s (ComponentInstance @%x)\n" indent (component-display-name instance) (System/identityHashCode instance))
       (format "%s  :props %s\n" indent (dissoc (props instance) :children))
       (format "%s  :state %s\n" indent (when-let [s (state instance)] @s))])))
(defmethod tree-level->str ReactElement [element level]
  (let [indent (apply str (repeat (* 2 level) " "))
        children (get-in element [:props :children])]
    (apply str
      (concat
        [(format "%s%s (Element(id=%x)@%x)\n" indent (element-display-name element) (get element :id) (System/identityHashCode element))
         (format "%s  :props %s\n" indent (dissoc (get element :props) :children))
         (let [s (get-state *updater* element)]
           (format "%s  :state@(%x) %s\n" indent (System/identityHashCode s) s))
         (format "%s  children (%d)\n" indent (count children))]
        (map (fn [child] (tree-level->str child (+ 2 level))) children)))))

(defn tree->str [v]
  (tree-level->str v 0))
