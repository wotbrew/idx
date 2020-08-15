# idx

This library provides clojure data structures (maps, sets and vectors) with on-demand secondary indexes. Providing 
alternative fast access paths to their elements.

## Features

- Wrappers for vectors, sets and maps.
- Index elements on demand by any property, such as: **functions** `(f element)`, **keys** `(get element k)` **paths** `(get-in element ks)` and anything satisfying the Property protocol.
- Indexes are computed on demand, cached transparently and reused for subsequent queries
- Indexes are maintained incrementally through `conj`, `assoc` and so on once cached.
  
## Usage

First include the artifact ```[com.wotbrew/idx "1.0.0"]``` or in deps `{com.wotbrew/idx {:mvn/version "1.0.0"}}`

All functions are available in the `com.wotbrew.idx` namespace.

### Wrap your collection

```clojure 
(idx coll)
```

This is very cheap, returns a new collection matching the type of the input. Implementations are provided for vectors, maps and sets.

No indexes are created at this point.

### Query your collection 

```clojure
;; one-to-many hash indexes, such as what you would get from (group-by). Unlike group-by the resulting sequences
;; are unordered.
(group coll :foo 42) ;; all elements where (:foo element) == 42
(group coll2 even? true) ;; all elements where (even? element) == true
(group coll2 even?) ;; short hand for all elements where (even? element) returns truthy.

;; one-to-one hash indexes for unique elements, cheaper than group when you have exactly one element for each value of the property.
(identify coll :id 0) ;; returns the single element where (:id element) == 0

;; sorted indexes 
(ascending coll :counter > 42) ;; all elements where (:counter element) > 42 in ascending order
(descending coll :counter < 42) ;; all elements where (:counter element) < 42 in descending order
```

Using a query function will construct and then cache the index needed to satisfy the query. The indexes are cached
against the collection instance, much like hash codes in clojure.

### Modify your collection

Any indexes cached against your collection will be maintained incrementally with new versions of the data structure.

Use normal clojure modification operators, `conj`, `assoc`, `into` whatever you would like.

As maintaining indexes can become expensive if you want to make a lot of modifications, you can unwrap the indexed structure
with `(unwrap coll)`.

## License

Copyright © 2020 Daniel Stone

Distributed under the [MIT](https://opensource.org/licenses/MIT) license.