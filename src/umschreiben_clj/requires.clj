(ns umschreiben-clj.requires
  (:refer-clojure :exclude [replace])
  (:require [clojure.set :as set]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.seq :as z.seq]))

(defn require-node->require-map [node]
  (let [zloc        (z/edn node)
        sexpr-after (fn [zloc after-value] (-> (z/next zloc)
                                               (z/find-value z/right after-value)
                                               z/right
                                               z/sexpr))]
    (if (z.seq/vector? zloc)
      {:namespace (z/sexpr (z/next zloc))
       :as        (sexpr-after zloc :as)
       :alias     (or (sexpr-after zloc :as)
                      (z/sexpr (z/next zloc)))
       :refer     (sexpr-after zloc :refer)}
      {:namespace (z/sexpr zloc)
       :alias     (z/sexpr zloc)})))

(defn- require-map->node [{:keys [namespace refer as] :as requires-map}]
  (let [in-map? #(boolean (get requires-map %))]
    (case (map in-map? [:namespace :as :refer])
      [true false false]
      `~namespace

      [true true true]
      `[~namespace :as ~as :refer ~refer]

      [true true false]
      `[~namespace :as ~as]

      [true false true]
      `[~namespace :refer ~refer]

      :else
      (throw (ex-info "can't build require entry from input" {:input requires-map})))))

(defn- find-function-call
  "Look for the first list used to call <symbol*> as in `(<symbol*> ...)`
  or `(apply <symbol*> ...)`. Returns nil if there's no such call."
  [zloc symbol*]
  (z/find-depth-first
   zloc
   (fn [current-zloc]
     (let [next-sexpr (comp z/sexpr z/next)]
       (and (z/list? current-zloc)
            (or (= symbol* (next-sexpr current-zloc))
                (and (= 'apply (next-sexpr current-zloc))
                     (= symbol* (next-sexpr (z/next current-zloc))))))))))

(defn- find-ns-call [file-zloc]
  (find-function-call file-zloc 'ns))

(defn- ns-body [file-zloc]
  (z/right (find-ns-call file-zloc)))

(defn- find-in [zloc path]
  (reduce #(z/find-value %1 z/next %2) zloc path))

(defn- replace-require [to]
  (fn [zloc] (z/replace zloc (require-map->node to))))

(defn- replace-refer-with-diff [zloc require-zloc existing-refer referrals-to-remove]
  (let [refer-diff (set/difference existing-refer referrals-to-remove)]
    (if (= existing-refer refer-diff)
      zloc
      (let [updated-refer-node (->> refer-diff
                                    vec
                                    sort
                                    vec
                                    not-empty
                                    (assoc require-zloc :refer)
                                    require-map->node)]
        (z/replace zloc updated-refer-node)))))

(defn- remove-moved-referrals [zloc referrals-to-remove symbol-to-replace?]
  (let [require-zloc    (-> zloc
                            z/sexpr
                            n/coerce
                            require-node->require-map)
        existing-refer  (:refer require-zloc)
        with-refer-all? (and (= :all existing-refer)
                             (not (empty? referrals-to-remove)))]
    (when with-refer-all?
      (throw (ex-info "If the symbol to be replaced is referred through :all you can't refer it in the new requirement"
                      {:referrals-to-remove referrals-to-remove})))
    ;; if the original `from` require's `:refer` list includes the symbol being renamed, include it
    ;; even if the destination `to` require uses `:as` without any `:refer`
    (let [referrals-to-remove-set (set/union (set referrals-to-remove)
                                             (when-not (= :all existing-refer)
                                               (set (filter symbol-to-replace? existing-refer))))]
      (if (not (empty? referrals-to-remove-set))
        (replace-refer-with-diff zloc require-zloc (set existing-refer) referrals-to-remove-set)
        zloc))))

(defn- add-require [to]
  (fn [zloc]
    (-> zloc
        (z/insert-right (require-map->node to))
        z/insert-newline-right)))

(defn- update-require [refers-to-remove symbol-to-replace?]
  (fn [zloc]
    (remove-moved-referrals zloc refers-to-remove symbol-to-replace?)))

(defn- assoc-some
  "Assoc[iate] if the value is not nil.

  Examples:
  ```
  (assoc-some {:a 1} :b false)
  ;=>
  {:a 1 :b false}

  (assoc-some {:a 1} :b nil)
  ;=>
  {:a 1}
  ```
  "
  ([m k v]
   (if (nil? v) m (assoc m k v)))
  ([m k v & kvs]
   (let [ret (assoc-some m k v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (IllegalArgumentException.
                 "assoc-some expects even number of arguments after map/vector, found odd number")))
       ret))))

(defn merge-existing+to-require-maps [to-require-map existing-require from-symbol to-symbol]
  (let [existing-refer    (:refer existing-require)
        new-refer         (:refer to-require-map)
        remove-from-refer (if (= from-symbol to-symbol)
                            identity
                            (fn [refers] (remove #(= from-symbol %) refers)))
        joined-refers     (if (or (= :all existing-refer) (= :all new-refer))
                            :all
                            (->> (concat existing-refer new-refer)
                                 (into #{})
                                 (into [])
                                 remove-from-refer
                                 not-empty))
        as                (or (:as existing-require)
                              (:as to-require-map))
        ns*               (:namespace to-require-map)]
    (assoc-some {:alias     (or as ns*)
                 :namespace ns*}
                :refer joined-refers
                :as as)))

(defn- requires-type->fn [to replace-requires-type symbol-to-replace? body-unchanged?]
  (if (and body-unchanged?
           (= :add replace-requires-type))
    (update-require (:refer to) symbol-to-replace?)
    (case replace-requires-type
      :replace (replace-require to)
      :merge   (replace-require to)
      :add     (add-require to))))

(defn- find-require [zloc ns-require]
  (-> zloc
      find-ns-call
      (find-in ['ns :require ns-require])))

(defn- requires-ns-matches? [sexpr require-ns]
  (= require-ns (if (coll? sexpr) (first sexpr) sexpr)))

(defn- update-requires-with-renames [requires-zloc to-refer symbol-to-replace? ns-to-replace]
  (let [update-duplicate (fn [zloc] (if (requires-ns-matches? (z/sexpr zloc) ns-to-replace)
                                      (remove-moved-referrals zloc to-refer symbol-to-replace?)
                                      zloc))]
    (loop [require-entry requires-zloc]
      (let [updated-entry (update-duplicate require-entry)]
        (if (z/right updated-entry)
          (recur (z/right updated-entry))
          (z/up updated-entry))))))

(defn- transform-header [zloc ns-to-replace replace-requires-type to body-unchanged? symbol-to-replace?]
  (let [replace-fn              (requires-type->fn to replace-requires-type symbol-to-replace? body-unchanged?)
        require-zloc            (find-require zloc ns-to-replace)
        vector-wrapped-require? (fn [zloc] (= (z/sexpr zloc)
                                              (-> zloc z/up z/down z/sexpr)))
        result                  (if (vector-wrapped-require? require-zloc)
                                  (replace-fn (z/up require-zloc))
                                  (replace-fn require-zloc))
        requires-to-update      (if (= :add replace-requires-type)
                                  result
                                  (z/right result))] ;; skip require at current zloc, we hit it already
    (or (update-requires-with-renames requires-to-update (:refer to) symbol-to-replace? ns-to-replace)
        result)))

(defn- from-namespaces? [requires value]
  (let [alias-set (set (map (comp str :alias) requires))
        refer-set (->> requires
                       (map :refer)
                       (filter coll?)
                       flatten
                       set)]
    (and (symbol? value)
         (or (alias-set (namespace value))
             (refer-set value)))))

(defn- symbols-from-namespace [old-requires symbol-to-replace? node]
  (if (= :reader-macro (n/tag (first node)))
    #{}
    (if-let [element (z/down node)] ;; check if it is a list jumping to the first element
      (let [f #(and (from-namespaces? old-requires %) (symbol-to-replace? %))]
        (->> element
             (iterate z/right)
             (take-while (comp not nil?))
             (remove #(= :uneval (z/tag %)))
             (map z/sexpr)
             (filter f)
             set))
      #{})))

(defn- replace-in-body
  [file-zloc new-require old-requires symbol-to-replace? node->value]
  (let [relevant-symbols (partial symbols-from-namespace old-requires symbol-to-replace?)
        replace-counter  (atom 0)
        replace+count    (fn [zloc]
                           (swap! replace-counter inc)
                           (z/replace zloc (node->value (z/node zloc) new-require (relevant-symbols zloc))))
        result           (loop [zloc file-zloc]
                           (let [new-zloc (z/prewalk zloc
                                                     #(not-empty (relevant-symbols %))
                                                     replace+count)]
                             (if (z/right new-zloc)
                               (recur (z/right new-zloc))
                               new-zloc)))]
    [(z/root result) @replace-counter]))

(defn- add-to-header [ns-zloc require*]
  (if-let [requires (z/find-next ns-zloc z/next (comp (partial = :require) z/sexpr))]
    (z/insert-right requires require*)
    (z/append-child ns-zloc (list :require require*))))

(defn use-require
  "Change <symbol*> to use <require*>.

  <require*> - Namespace, alias and refer information
  <symbol*>  - Symbol

  Usage:

    (use-require {:namespace 'a.b.c} 'x/myfn) ;=> a.b.c/myfn

    (use-require {:alias 'c} 'x/myfn) ;=> c/myfn

    (use-require {:refer ['myfn]} 'x/myfn) ;=> myfn"
  [{alias* :alias refer* :refer namespace* :namespace} symbol*]
  (let [symbol-name (symbol (name symbol*))]
    (cond
      (or (= :all refer*) (some #{symbol-name} refer*))
      symbol-name

      alias*
      (symbol (str alias* "/"  symbol-name))

      (not (or alias* refer*))
      (symbol (str namespace* "/"  symbol-name))

      :else
      symbol-name)))

(defn- map-seq
  [f zloc]
  (if-let [zloc-n0 (z/down zloc)]
    (let [move-right+apply (fn [loc] (when-let [zloc-n (z/right loc)]
                                       (f zloc-n)))]
      (some->> (f zloc-n0)
               (iterate move-right+apply)
               (take-while identity)
               last
               z/up))
    zloc))

(defn replace-using-require
  "Returns a modified version of <node> where <usage> conforms to <require*>.

  <node>    - Node containing <usage>
  <require> - Map containing namespace, alias and refer information
  <usages>  - Set of Symbols to replace

  Usage:

  (replace-using-require
    identity
    (n/coerce '(apply x/myfn [1 2]))
    {:namespace 'a.b.c :alias 'c}
    #{'x/myfn}) ;=> (apply c/myfn [1 2])"
  [transform-symbol node require usages]
  (let [zloc (z/edn node)
        replace-usage (fn [element]
                        (let [expr (z/sexpr element)]
                          (if (usages expr)
                            (z/replace element (use-require require (transform-symbol expr)))
                            element)))]
    (if (z/map? zloc)
      (->> zloc (z/map-keys replace-usage) (z/map-vals replace-usage) z/node)
      (->> zloc (map-seq replace-usage) z/node))))

(defn- transform-body [zloc new-require old-requires symbol-to-replace? replace-fn]
  (if-let [body-zloc (-> zloc z/root z/edn ns-body)]
    (replace-in-body body-zloc
                     new-require
                     old-requires
                     symbol-to-replace?
                     replace-fn)
    [(z/root zloc) 0]))

(defn- find-requires [zloc ns-require]
  (let [req (-> zloc
                find-ns-call
                (find-in ['ns :require]))]
    (->> req
         (iterate z/right)
         (take-while identity)
         (filter #(requires-ns-matches? (z/sexpr %) ns-require))
         (map z/sexpr))))

(defn- lookup-require-maps [file-zloc namespace]
  (->> (find-requires file-zloc namespace)
       (map n/coerce)
       (map require-node->require-map)))

(defn transform-header-and-body [file-node from-ns to replace-requires-type replace-fn symbol-to-replace?]
  (let [file-zloc     (z/edn file-node)
        ns-to-replace (if (= :merge replace-requires-type)
                        (:namespace to)
                        from-ns)]
    (when (find-require file-zloc ns-to-replace)
      (let [old-requires        (lookup-require-maps file-zloc from-ns)
            [transformed-body
             replace-count]     (transform-body file-zloc to old-requires symbol-to-replace? replace-fn)]
        (-> transformed-body
            z/edn
            (transform-header ns-to-replace replace-requires-type to (zero? replace-count) symbol-to-replace?)
            z/root)))))

(defn replace
  "Replaces the require of namespace <from> with <to> and also replaces all
  usages of it accordingly to <to>, applying alias and :refer symbols if they
  are present. Returns the root Node of <file-zoc>.

  <file-node>  - Node of an entire namespace
  <from>       - Symbol of a namespace
  <to>         - code requiring a namespace
  <replace-fn> - optional function to be applied to every usage, if any, of the
                 required namespace in <file-node>. If not provided,
                 `replace-using-require` will be used.

  <replace-fn> format:

   (fn [node require usages] ... ) ;=> must return a node or s-expr

  where

  <node>    - Node containing the list where <usages> are being called. This
              node will be swapped in <file-node> by the return value of
              <replace-fn>
  <require> - Require expression. Same as <to>
  <usages>  - Symbol that belongs to <to>

  Usage:

    ; simple usage
    (replace (z/of-string ...) 'a.b.c '[x.y.z :as z :refer [t]])

    ; providing a custom replace-fn
    (replace
     (z/of-file ... )
     'a.b.c
     '[x.y.z :as z :refer [t]]
     (fn [root-node require-node usage] ... ))"
  ([file-node from to]
   (replace file-node
            from
            to
            (partial replace-using-require identity)))
  ([file-node from to replace-fn]
   (let [to-require-map (-> to
                            n/coerce
                            require-node->require-map)]
     (transform-header-and-body file-node from to-require-map :replace replace-fn (constantly true)))))

(defn add
  "Add <require*> to <file-node>. Returns the root Node of <file-node>.

  If <file-node> already requires the namespace of <require*>, change all of its
  usages accordingly to <require*>, applying alias and :refer symbols if they
  are present.

  <file-node>  - Node of an entire namespace
  <require>    - code requiring a namespace
  <replace-fn> - optional function to be applied to every usage, if any, of the
                 required namespace in <file-node>. If not provided,
                 `replace-using-require` will be used.

  <replace-fn> format:

   (fn [node require usages] ... ) ;=> must return a node or s-expr

  where

  <node>    - Node containing the list where <usages> are being called. This
              node will be swapped in <file-node> by the return value of
              <replace-fn>
  <require> - Require expression. Same as <to>
  <usages>  - Symbol that belongs to <to*>

  Usage:

    ; simple usage
    (add (z/of-string ...) '[x.y.z :as z :refer [t]])

    ; providing a custom replace-fn
    (add
     (z/of-string ... )
     '[x.y.z :as z :refer [t]]
     (fn [root-node require-node usages] ... ))"
  ([file-node require*]
   (add file-node
        require*
        (partial replace-using-require identity)))
  ([file-node require* replace-fn]
   (let [require-zip (z/of-string (str require*))
         require-ns  (z/sexpr (or (z/down require-zip)
                                  require-zip))]
     (if-let [root-node (replace file-node require-ns require* replace-fn)]
       root-node
       (-> (z/edn file-node)
           find-ns-call
           (add-to-header require*)
           z/root)))))
