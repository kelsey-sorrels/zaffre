(ns zaffre.components
  (:require clojure.data
            clojure.string
            [clojure.zip :as zip]
            [overtone.at-at :as atat]
            [taoensso.timbre :as log]))
 
(def ^:dynamic *pool* (atat/mk-pool))
(declare ^:dynamic *current-owner*)

(declare element?)
(declare element-id-str)

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
                     (if (and (map? v1) (map? v2))
                       (merge-with deep-merge v1 v2)
                       v2))]
    (when (some identity vs)
      (reduce #(rec-merge %1 %2) v vs))))

;; State
(defprotocol IUpdater
  (immediate-set-state! [this element partial-state])
  (enqueue-set-state! [this element partial-state callback])
  (enqueue-remove-state! [this element])
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
  (immediate-set-state! [this element partial-state]
    {:pre [(element? element) (get element :id)
           (map? partial-state)]}
    (let [k (get element :id)]
      (swap! a (fn [updater]
                 (update-in updater [:s k] #(deep-merge % partial-state))))
      this))
  (enqueue-set-state! [this element partial-state callback]
    {:pre [(element? element) (get element :id)
           (or (map? partial-state)
               (fn? callback))]}
    (let [k (get element :id)
          callback (cond
                     (map? partial-state)
                       (fn partial-state-callback [s]
                         {:pre [(some? s)]
                          :post [(some? %)
                                 (map? %)]}
                         (try
                           (log/trace "partial-state-callback" (format "%x" k) partial-state)
                           (update s k #(deep-merge % partial-state))
                           (catch Exception e
                             (log/error "Exception applying callback" e)
                             s)))
                     (fn? callback)
                       (fn fn-callback [s]
                         {:pre [(some? s)]
                          :post [(some? %)
                                 (map? %)]}
                         (try
                           (log/trace "fn-callback" s k callback)
                           (update s k (fn [s] (deep-merge s (callback s))))
                           (catch Exception e
                             (log/error "Exception applying callback" e)
                             s)))
                     :else
                       (assert false (str "Got neither partial-state nor callback" partial-state callback)))]
      (log/trace (format "enqueue-set-state! enqueued update for element %x" k))
      (swap! a (fn [{:keys [q] :as updater}]
                 (assoc updater :q (conj q callback)))))
      this)
  (enqueue-remove-state! [this element]
    {:pre [(element? element) (get element :id)]}
    (let [k (get element :id)
          callback (fn [s] (dissoc s k))]
      (swap! a (fn [{:keys [q] :as updater}]
                 (assoc updater :q (conj q callback)))))
      this)
  (update-state! [this]
    (swap! a (fn [{:keys [q s] :as updater}]
               (assert q (str "q cannot be nil" q s))
               (assert (some? s) (str "s cannot be nil" q s))
               ;(when-not (zero? (count q))
                 (log/trace "update-state!" (count q) "queued updates")
               ;; apply queued changes to old state to make new state
               (let [new-s (reduce (fn [s f]
                                     {:pre [(some? s) (some? f)]
                                      :post [(some? %)]}
                                     (f s)) s q)]
                 (assert (some? new-s) (str "new-s cannot be nil q:" q "s:" s "new-s:" new-s))
                 (assoc updater :q []
                                :s new-s
                                :prev-state s))))
    this)
  (element-state-changed? [this element]
    (assert (some? element) "Element must not be nil")
    (assert (-> element :id some?) "Element must have :id")
    (let [k (get element :id)
          {:keys [s prev-state] :as updater} @a
          current-state (get s k)
          prev-state (get prev-state k)]
      (log/trace "element-state-changed?\nprev-state: " prev-state "\ncurrent-state: " current-state "\nchanged?: " (not= prev-state current-state))
      (not= current-state prev-state)))
  (get-state [this element]
    (let [k (get element :id)]
      (-> a deref :s (get k))))
  (get-prev-state [this element]
    (let [k (get element :id)]
      (-> a deref :prev-state (get k)))))

(defn empty-state [] (->Updater (atom {:q [] :s {} :prev-state {}}
                                      :validator (fn [{:keys [q s prev-state]}]
                                                   (and (vector? q)
                                                        (every? fn? q)
                                                        (map? s)
                                                        (map? prev-state))))))

(defrecord NoopUpdater []
  IUpdater
  (immediate-set-state! [this element partial-state]
    (assert (-> element :id some?) "Element must not be nil")
      this)
  (enqueue-set-state! [this element partial-state callback]
    (assert (-> element :id some?) "Element must not be nil")
      this)
  (enqueue-remove-state! [this element]
    (log/error "Removing noop state")
    this)
  (update-state! [this]
    (log/error "Updating noop state")
    this)
  (element-state-changed? [this element]
    (assert (-> element :id some?) "Element must not be nil")
    false)
  (get-state [this element] nil)
  (get-prev-state [this element] nil))

(def noop-updater (->NoopUpdater))

(defrecord AlwaysThrowUpdater []
  IUpdater
  (immediate-set-state! [this element partial-state]
    (assert false))
  (enqueue-set-state! [this element partial-state callback]
    (assert false))
  (enqueue-remove-state! [this element]
    (assert false))
  (update-state! [this]
    (assert false))
  (element-state-changed? [this element]
    (assert false))
  (get-state [this element]
    (assert false))
  (get-prev-state [this element]
    (assert false)))

(def always-throw-updater (->AlwaysThrowUpdater))

(def ^:dynamic *updater* always-throw-updater #_noop-updater)

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
  (set-state! [this update])
  (force-update! [this callback]))

(defprotocol ComponentProperties
  (default-props [this])
  (display-name [this]))

(defprotocol ConstructorMethods
  (create-instance [this] [this state]))

(defprotocol InstanceProperties
  (props [this])
  (state [this]))


(declare ->ComponentInstance)
(defrecord Component [component-will-mount
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
  (set-state! [this update]
    (if (or (map? update) (fn? update))
      (let [partial-state (when (map? update) update)
            callback (when (fn? update) update)]
        (log/trace "set-state! " (element-id-str *current-owner*))
        (enqueue-set-state! *updater* *current-owner* partial-state callback))
      (throw "setState(...): takes an object of state variables to update or a
             function which returns an object of state variables.")))
  (force-update! [this callback]
    (enqueue-set-state! *updater* *current-owner* nil callback))

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

;; From https://reactjs.org/docs/events.html
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
                                :on-animation-iteration
                                ;; form events
                                :on-change})

(defn is-valid-event-handler-key? [k]
  (contains? valid-event-handler-keys k))

(def default-opts {
  :component-will-mount (fn [this] nil)
  :render (fn [this] nil)
  :component-did-mount (fn [this] nil)
  :component-will-receive-props (fn [this next-props] nil)
  :should-component-update? (fn [this next-props next-state] true)
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
      (->Component component-will-mount
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

(defprotocol PartialHash
  (hash-type-and-props [this]))

;; ReactElement
(defrecord ReactElement [id type key ref self source owner props hash-cache]
  Object
  (toString [this]
    (format "ReactElement: %s :props %s :state %s"
      (if (keyword? (get this :type))
        (name (get this :type))
        (display-name (get this :type)))
      props
      "unknown"  #_(get-state *updater* this)))
  PartialHash
  (hash-type-and-props [this]
    (log/info "id" id "type" (if (component? type) (display-name type) type))
    ;(log/info *updater*)
    (log/info (get-state *updater* this))
    (if-let [h @hash-cache]
      h
      (reset! hash-cache
        (bit-xor
         ;(rand-int 10000)
         ; type
         (hash type)
         ; state
         (hash (or (get-state *updater* this) 0))
         ; props - recurse through children
         (hash
           (update props :children
             (fn [children]
               (reduce bit-xor
                 0
                 (map (fn [child]
                   (if (instance? ReactElement child)
                     (hash-type-and-props child)
                     (hash child)))
                   children))))))))))

(defn element-id-str [element]
  {:pre [(element? element) (get element :id)]}
  (format "%x" (get element :id)))

(defn element-children [element]
  (get-in element [:props :children]))

(defn assoc-children [element children]
  (assoc-in element [:props :children] children))

(defn element-without-children [element]
  (assoc-children element []))

;; TODO: match children based on keys
(defn map-children [f element & more]
  "Maps element's childrend and more's children calling (f element-child & more-children).
   Returns updated children."
  ;; `more` may contain nil elements. nil elements return infinite nil children
  (letfn [(element-children-or-nil [element] (concat (element-children element) (repeat nil)))]
    (let [elements (cons element more)
          children (map element-children-or-nil elements)
          #_ (assert (every? coll? children) (str "Non collection children " (vec (map type children))))
          children (apply map vector children)
          children (vec (take-while (fn [v] (not-every? nil? v)) children))
          new-children (into [] (map (fn [child-vec] (apply f child-vec)) children))]
      new-children)))

(defn map-in-children [f element & more]
  "Maps element's childrend and more's children calling (f element-child & more-children).
   Returns element with updated children."
  (assoc-in element [:props :chilren] (apply map-children f element more)))

(defn component? [v]
  (instance? Component v))

(defn element? [v]
  (and (vector? v)
       (= (count v) 4)
       (keyword? (first v))
       (map? (second v))))

(defn- valid-children? [type children]
  (assert (sequential? children) (format "%s not sequential" children))
  (if (= type :img)
    (letfn [(child-validator [child] (and
                                       (sequential? child)
                                       (every? map? child)))]
      (assert (every? child-validator children)
              (str " Not every child is a pixel in " type " "
                (vec (filter (complement child-validator) children)))))
    (letfn [(child-validator [child] (or (component? child)
                                         (element? child)
                                         (string? child)
                                         (nil? child)))]
      (assert (every? child-validator children)
          (str " Not every child is a component or element in " type " "
               (first (filter (complement child-validator) children))))))
  true)

;; React.createElement(Hello, {toWhat: 'World'}, null)
(defn create-element [type config children & more]
  {:post [(element? %)]}
  (assert (every? is-valid-event-handler-key?
                  (filter (fn [k] (clojure.string/starts-with? (name k) "on-"))
                          (keys config)))
    (str "Found unsupported event"
         (vec (remove is-valid-event-handler-key?
                  (filter (fn [k] (clojure.string/starts-with? (name k) "on-"))
                          (keys config))))))
  (let [ref    (if (and config (has-valid-ref? config)) (get config :ref))
        key    (if (and config (has-valid-key? config)) (get config :key))
        self   (get config :self)
        source (get config :source)
        ;; either a single children array arg or multiple children using & more
        children (remove nil? (if (not (empty? more)) (cons children more) children))
        #_ (assert (valid-children? type children))
        props  (deep-merge
                    {:children children}
                    (if type (get type :default-props {}) {})
                    (into {}
                      (remove (fn [[prop-name _]]
                                (reserved-prop? prop-name)))
                              config))
        element (ReactElement. nil type key ref self source nil props (atom nil))]
    (assoc element :id (System/identityHashCode element))))

(defn create-factory [type]
  (partial create-element type))

;; cloneAndReplaceKey
(defn assoc-key [element key]
  (assoc element :key key))

(defn- csx* [v]
  (if-not (vector? v)
    v
    (do
      (log/trace "expanding" v)
      (let [[type props children] (case (count v)
                                    1 [(first v) {} []]
                                    2 [(first v) (second v) []]
                                    3 [(first v) (second v) (last v)]
                                    [(first v) (second v) (drop 2 v)])
             csx-children (if (and children (or (symbol? children) (list? children)))
                            children
                            (map csx* children))
             #_#_display-name (display-name type)
             ; TODO: is there a better way to calculate display-name?
             config props]
           (list `create-element
                  type
                  config
                  (if (or (symbol? csx-children) (list? csx-children))
                    csx-children
                    (cons list csx-children)))))))
  
(defmacro csx [v]
  (let [r (csx* v)]
    (log/trace "csx" r)
    r))

(defmacro def-component [sym & fn-tail]
  "(def-component red-text [this] (zc/csx [:text (props this) [\"label\"]]))"
  (let [sym# (symbol sym)
        display-name# (str sym#)]
    (println "Defining component" sym# display-name#)
    (list 'def sym# (list fn->component
               `(fn ~sym ~@fn-tail)
               (or display-name# "Unknown")))))

(defn component-display-name [component]
  (cond
    (keyword? component)
      (name component)
    :default
      (display-name component)))

(defn element-display-name [element]
  (cond
    (and (some? element) (get element :type))
      (component-display-name (get element :type))
    (string? element)
      "String"
    (nil? element)
      "nil"
    :else
      (assert false element)))

(defn construct-instance
  [element]
  {:pre [(element? element)]
   :post [(component-instance? %)]}
  (log/trace "constructing component instance" (element-display-name element) (element-id-str element))
  (log/trace "*updater*" *updater*)
  (let [{:keys [type props]} element
        state (or (get-state *updater* element)
                  (when (component? type)
                    (when-let [s ((get type :get-initial-state))]
                      (log/trace "construct-instance state" s)
                        (immediate-set-state! *updater* element s)
                        s)))
        _ (log/trace "construct-component element type" (component-display-name type) state)
        instance (if (fn? type)
                   (create-instance (assoc (fn->component type) :props props))
                   (create-instance (assoc type :props props) state))]
    instance))


;; Transform element tree into a sequence by walking the tree
(defn element-tree-seq [element]
  (let [element-seq (tree-seq element?
                              element-children
                              element)]
    element-seq))

;; Return only the leaf nodes from text-tree-seq
(defn filter-element-tree [p element]
  (let [elements (element-tree-seq element)]
    (filter p elements)))

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

(defn tree-edges
  ([loc]
    (tree-edges loc true))
  ([loc rightmost]
    (if loc
      (let [has-right-sibling (some? (zip/right loc))]
        (conj (tree-edges (zip/up loc) false)
              (cond
                (and rightmost has-right-sibling)
                  "\u251c" ;├
                (and rightmost (not has-right-sibling))
                  "\u2514" ; └
                (and (not rightmost) has-right-sibling)
                  "\u2502" ; │
                (and (not rightmost) (not has-right-sibling))
                  " ")))
              
      [])))

(defn zipper-elements
  [root-element]
  (zip/zipper
    ; can have children
    (fn [v] (or (element? v) (map? v) (map-entry? v)))
    ; children
    (fn [v]
      (cond
        (element? v)
          (vec (array-map
            :props (nth v 1)
            :host-dom @(nth v 3)
            :children (nth v 2)))
        (map? v)
          (vec v)
        (map-entry? v)
          (if (coll? (second v))
            (second v)
            [(second v)])))
    ; with children
    (fn [v children]
      (cond
        (element? v)
          (let [[tag props _ host-dom] v]
            [tag props children host-dom])
        (map? v)
          {(ffirst v) children}
        (map-entry? v)
          (first {(first v) children})))
    root-element))


(defn zipper-descendants
  [z]
  #_(log/info "zipper-descendants" z)
  (if-not (zip/end? z)
    (lazy-seq
      (cons z (zipper-descendants (zip/next z))))
    []))

(defn tree->str [root-element]
  (clojure.string/join "\n"
    (cons "\nroot-element"
      (for [z (-> root-element zipper-elements zipper-descendants)
            :let [node (zip/node z)]]
        (str (clojure.string/join (tree-edges z))
             (cond
               (element? node)
                 (str (first node) (nth node 3) (nth node 4))
               (map-entry? node)
                 (first node)
               (map? node)
                 {}
               (nil? node)
                 "nil"
               :else
                 node))))))
  
(defn -main [& args]
  (println 
    (clojure.string/join "\n"
      (cons "\nX"
        (for [z (-> (first {:root {:a [1 2] :b {3 "x"} :c "c"}}) zipper-elements zipper-descendants)
              :let [node (zip/node z)]]
          (str (clojure.string/join (tree-edges z))
               (cond
                 (element? node)
                   (first node)
                 (map-entry? node)
                   (first node)
                 (map? node)
                   {}
                 :else
                   node)))))))
