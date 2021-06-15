(ns umschreiben-clj.requires
  (:refer-clojure :exclude [replace])
  (:require [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [umschreiben-clj.internals.requires :as internals.requires]))

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
                            (z/replace element (internals.requires/use-require require (transform-symbol expr)))
                            element)))]
    (if (z/map? zloc)
      (->> zloc (z/map-keys replace-usage) (z/map-vals replace-usage) z/node)
      (->> zloc (internals.requires/map-seq replace-usage) z/node))))

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
                            internals.requires/require-node->require-map)]
     (internals.requires/transform-header-and-body file-node from to-require-map :replace replace-fn (constantly true)))))

(defn add
  "Add <require*> to <file-node>. Returns the root Node of <file-node>.

  If <file-node> already requires the namespace of <require*>, change all of its
  usages accordingly to <require*>, applying alias and :refer symbols if they
  are present.

  <file-node>  - Node of an entire namespace
  <require>    - code requiring a namespace
  <replace-fn> - optional function to be applied to every usage, if any, of the
                 required namespace in <file-node>. If not provided,
                 `internals.requires/replace-using-require` will be used.

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
           internals.requires/find-ns-call
           (internals.requires/add-to-header require*)
           z/root)))))
