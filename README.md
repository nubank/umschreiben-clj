# umschreiben-clj

Rewrite utilities for refactoring clojure files.

Mostly a collection of useful extensions written on top of [`rewrite-clj`](https://github.com/clj-commons/rewrite-clj).

This is an experimental alpha-stage library that is sure to have many bugs and coverage issues.
Please be patient and feel free to contribute issues and fixes.

## functionality

### managing requires of a namespace

In the `umschreiben-clj.requires` namespace you'll find:

#### `replace`
Replaces the require of the namespace `<from>` with `<to>`, also replacing all usages of `<from>` accordingly to `<to>`, applying alias and :refer symbols if they are present.

#### `add`
Able to add a require to the list of requires of a namespace. If the namespace being added already exists in the list of requires, `add` behaves like `replace`.

### renaming variables across namespaces

in the `umschreiben-clj.variables` namespace you'll find:

#### `rename`
Allows one to rename a variable that may or may not have also live in a new namespace.

```clojure
(umschreiben-clj.variables/rename ..node-representation-of-clj-ns.. 'clojure.string/join 'my-join '[my.helpers :as helpers])
```

run on

```clojure
(ns rename-example
  (:require [clojure.string :as str]))
(str/join ["hello" "world"])
```

results in

```clojure
(ns rename-example
  (:require [clojure.string :as str] ;; left in-place in case other usages are present
            [my.helpers :as helpers]))
(helpers/join ["hello" "world"])
```

Depending on the structure of the namespaces `:require`s the behavior of `umschreiben-clj.variables/rename` differs widely so we encourage one to check out the examples in the test suite and also play around with it yourself.

#### known issues

Renaming variables that are required using `:refer` has limited support and isn't recommended.
Since `umschreiben-clj` doesn't consider binding information, renaming un-namespaced variables that are `:refer`'ed from other namespaces can conflict with local variable bindings.
For example, running `(umschreiben-clj.variables/rename ..node-of-code-below.. 'clojure.string/join 'my-join '[my.helpers :as helpers])` on:

```clojure
(ns shadowing-issue
  (:require [clojure.string :refer [join]]))
(let [join "hello world"]
  (println join))
```

currently results in the incorrect result:

```clojure
(ns shadowing-issue
  (:require clojure.string
            [my.helpers :as helpers]))
(let [helpers/join "hello world"]
  (println helpers/join))
```

## examples in use

At Nubank this library, used in conjuction with [`ordnungsamt`](https://github.com/nubank/ordnungsamt/), allows us to realize namespace and function deprecations across hundreds of Clojure repositories

## contributing

Please feel encouraged to contribute to this library by either opening an issue or contributing a patch.

### tests

If you introduce a new feature please provide test coverage of it.

Tests can be run using:
```
lein test
```

### linting

Before opening a pull request, please check that the linter is happy

```
lein lint
```

One can apply lint fixes via

```
lein lint-fix
```
