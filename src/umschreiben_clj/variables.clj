(ns umschreiben-clj.variables
  (:require [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [umschreiben-clj.internals.requires :as internals.requires]
            [umschreiben-clj.requires :as requires]))

(defn rename
  "Renames <from> variable usages in a namespace to be <to>.

  If <from> and <to> are in different namespaces, the <to> namespace
  will be included in `:require`s if needed.


  <file-node>    - Node of an entire namespace
  <from>         - Symbol of a namespace
  <to-symbol>    - Non-namespaced symbol to replace <from>
  <to-require>   - code requiring a namespace

  Usage:

    ; simple usage
    (rename (z/of-string ...) 'a.b.c/config 'new-config '[a.b.c :as c])

    ; with aliasing
    (rename (z/of-string ...) 'a.b.c/config 'config '[x.y.z :as z :refer [config]])
  "
  [file-node from to-symbol to-require]
  (assert (nil? (namespace to-symbol))
          "target of rename shouldn't be namespaced")
  (let [from-ns          (-> from namespace symbol)
        from-symbol      (-> from name symbol)
        to-require-map   (-> to-require
                             n/coerce
                             internals.requires/require-node->require-map)
        existing-require (-> file-node
                             z/edn
                             (internals.requires/lookup-require-maps (:namespace to-require-map))
                             first)
        transform-symbol #(get {from-symbol to-symbol} (-> % name symbol))
        to-require       (internals.requires/merge-existing+to-require-maps
                          to-require-map existing-require from-symbol to-symbol)
        update-type      (if (:namespace existing-require)
                           :merge
                           :add)]
    (internals.requires/transform-header-and-body file-node
                                                  from-ns
                                                  to-require
                                                  update-type
                                                  (partial requires/replace-using-require transform-symbol)
                                                  transform-symbol)))
