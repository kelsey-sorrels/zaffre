(ns zaffre.override)

(defn- parse-impls [specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))


(defn factory-fn-name [t]
  "Where `t` is a type ie a class"
  (let [parts (-> t .getCanonicalName (clojure.string/split #"\."))
        package (butlast parts)
        nom     (last parts)]
    (str (clojure.string/join "." package) "/->" nom)))

(defn identity-impl
  [parent-sym sigs]
  (vec
    (mapcat (fn [{:keys [name arglists]}]
              (mapv (fn [arglist]
                      (list name arglist (concat (list name parent-sym) (rest arglist))))
                    arglists))
         (vals sigs))))

(defn factory-fn-symbol [v]
  (-> v factory-fn-name symbol))

(defn merge-impls [& impls]
  (apply merge-with (fn [result later]
                      (let [result-map (group-by first result)
                            later-map  (group-by first later)
                            merged     (merge result-map later-map)]
                        #_(println "result-map" result-map)
                        #_(println "later-map" later-map)
                        #_(println "merged" merged)
                        (->> merged vals (mapcat identity))))
                    impls))

(defmacro override [[sym v] & specs]
  #_(println sym (type sym))
  (let [record-name (gensym "Record")
        overrides   (parse-impls specs)
        protocols   (keys overrides)
        defaults    (into {}
                          (map (fn [p]
                                 (println "sigs" p (-> p resolve #_deref :sigs) sym)
                                 [p (identity-impl sym (-> p resolve #_deref :sigs))]) 
                               protocols))
        impls       (mapcat (fn [[k v]] (cons k v)) (merge-impls defaults overrides))]
    (println "record-name" record-name)
    (println "overrides" overrides)
    (println "protocols" protocols)
    (println "defaults" defaults)
    (println "impls" impls)
    (clojure.pprint/pprint 
      `(let [~sym          ~v
             record-type# (defrecord ~record-name [~sym]
                            ~@impls)]
         ((find-var (factory-fn-symbol record-type#)) ~v)))
    `(let [~sym          ~v
           record-type# (defrecord ~record-name [~sym]
                          ~@impls)]
       ((find-var (factory-fn-symbol record-type#)) ~v))))
  
(comment
(use 'robinson.override :reload)

(defprotocol S
  (foo [this])
  (bar [this]))

(defprotocol T
  (baz [this] [this v]))

(defprotocol U
  (qux [this]))

(defrecord R [v]
  S
  (foo [this] (println "foo" v))
  (bar [this] (println "bar" v))
  T
  (baz [this] (println "baz" v))
  (baz [this w] (println "baz" v w))
  U
  (qux [this] (println "qux" v)))

(def r (->R "123"))

(doto r
  foo
  bar
  baz
  (baz :a)
  qux)

(macroexpand-1 '(override [parent r] S (foo [this] (println "r-proxy foo" parent))))

(override [parent r] S (foo [this] (println "r-proxy foo" parent))
                     T
                     U)

(def r-override (override [parent r]
                  S
                  (foo [this] (println "r-proxy foo" parent))
                  #_(bar [this] (println "r-proxy bar" parent))
                  T
                  U))

(doto r-override
  foo
  bar
  baz
  (baz :a)
  qux)

)
