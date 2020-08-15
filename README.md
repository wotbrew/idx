# idx

This library provides clojure data structures (maps, sets and vectors) with on-demand secondary indexes. Providing 
alternative fast access paths to their elements.

## Features

- Wrappers for vectors, sets and maps.
- Index elements on demand by any property, such as clojure functions, keywords, paths, selections. See [reference](#built-in-properties)
- Indexes are cached transparently and reused for subsequent queries
- Indexes are maintained incrementally through `conj`, `assoc` and so on once cached.

## Caveats

- No ClojureScript support, Clojure only for now. (pull requests welcome!)
- If you are only querying once, it is always faster to just use sequence functions than build indexes.
- For small n indexes are very expensive. Use it to flatten quadratic joins, do not use it to replace all sequence filtering.
- If you index by function, that function must absolutely be pure, otherwise all bets are off. Similar to comparators and (sorted-set-by).

## Usage

All functions are available in the `com.wotbrew.idx` namespace.

### Wrap your collection

```clojure 
(idx coll)
```

This is very cheap, returns a new collection matching the type of the input. Implementations are provided for vectors, maps and sets.

If you pass in an already indexed collection, it is returned as is.

### Query your collection 

```clojure

(def coll (idx [{:foo 42, :id 1, :counter 453}, {:foo 42, :id 0, :counter 23}, {:foo 43, :id 2, :counter 43}]))
(def coll2 (idx (vec (range 100))))

;; one-to-many hash indexes, such as what you would get from (group-by). 
;; Unlike group-by the resulting sequences are unordered.
(group coll :foo 42)
;; =>
({:foo 42, :id 0, :counter 23}, {:foo 42, :id 1, :counter 453})

;; can query by a function (careful of equality)
(group coll2 even? true) 
;; => 
(0,2,4 ...)

;; 2-ary form for truthyness index
(group coll2 even?)
;; =>
(0,2,4 ...)

;; one-to-one hash indexes for unique elements, cheaper than group when you have exactly one element for each value of the property.
(identify coll :id 0) 
;; => 
{:foo 42, :id 0, :counter 23}

;; sorted indexes 
(ascending coll :counter > 42) ;; all elements where (:counter element) > 42 in ascending order
;; => 
({:foo 43, :id 2, :counter 43}, {:foo 42, :id 1, :counter 453})

(descending coll :counter < 42) ;; all elements where (:counter element) < 42 in descending order
;; =>
({:foo 42, :id 0, :counter 23})
```

Using a query function will construct and then cache the index needed to satisfy the query. The indexes are cached
against the collection instance, much like hash codes in clojure.

### Modify your collection

Any indexes cached against your collection will be maintained incrementally with new versions of the data structure.

Use normal clojure collection functions such as `conj`, `assoc`, `dissoc`, `disj` and `into`. 

As maintaining indexes can become expensive if you want to make a lot of modifications, you can unwrap the indexed structure
with `(unwrap coll)`.

## Reference

### Built-in Properties

- functions
- paths with `(path ks)` for `(get-in element ks)`
- key selections with `(select ks)` for `(select-keys ks)`
- any other object is looked up with `(get element o)`
- escape functions to `(get element o)` with `(as-key o)`

## License

Copyright © 2020 Daniel Stone

Distributed under the [MIT](https://opensource.org/licenses/MIT) license.