# idx

This library provides clojure data structures (maps, sets and vectors) with on-demand secondary indexes. Providing 
alternative fast access paths to their elements.

## Features

- Wrappers for vectors, sets and maps.
- Index elements on demand by any property, such as clojure functions, keywords, paths, selections. See [reference](#properties)
- Indexes are cached transparently and reused for subsequent queries
- Indexes are maintained incrementally through `conj`, `assoc` and so on once cached.

## Caveats

- No ClojureScript support, Clojure only for now. (pull requests welcome!)
- If you are only querying once or twice, it is almost always faster to just use sequence functions than build indexes.
- For small n indexes are very expensive. Use it to flatten quadratic joins, do not use it to replace all sequence filtering.
- If you index by function, that function must absolutely be pure, otherwise all bets are off. Similar to comparators and (sorted-set-by).
- Each index use memory proportional to the collection size. Its best not to throw lots of queries requiring distinct indexes at a single collection. Because of this it is best to use the wrapper only in closed situations where you account for all the queries made against it.

## Usage

All functions are available in the `com.wotbrew.idx` namespace.

### Wrap your collection

```clojure 
(idx coll)
```

This is very cheap, returns a new collection matching the type of the input. Implementations are provided for vectors, maps and sets.

If you pass in an already indexed collection, it is returned as is. 

It will behave like a normal data vector/map/set and you can use standard modification functions.

### Query your collection 

Once wrapped with idx, a small suite of functions is available to query your collection.

#### `group`

Establishes a one-to-many hash index.

Returns vectors of elements. You pass a predicate or property to test and value to match.

Say you have a vector of users, each with an age key, you might do:

`(group users :age 10)` 

Say you have a vector of numbers, and you want to find the negative ones, functions are both properties and predicates.

`(group numbers neg? true)` or `(group coll neg?)`.

#### `identify`

Establishes a one-to-one hash index

`identify` operates on unique properties and predicates. It will throw 
if the property is non unique. Good for by-id type queries.

`(identify users :id 32344)`

#### `ascending`, `descending`

Establishes a one-to-many sorted index.

#### `path`

`path` allows nested indexes, use it for nested indexes as if you
are indexing a get-in call.

`(group orders (path :user :id) 32344)`

#### `match`

`match` composes properties to form composite indexes. 

`(group numbers (match neg? true, even? true))`

They keys can be any property, and match nests.

This allows for some pretty extensive (and expensive!) indexes, but is useful to compose 
a couple of properties together for joins.

```clojure
(group orders (match (path :user :id) 32444,
                     :carrier (match :country "uk" :available true)
                     :delivery-date #"2020-08-17"))
```

#### `pred`

`pred` allows you to place truthyness/falseyness tests in the value position(s) of `match`, `group` and `identify` calls.

`(match :foo (pred even?))`

#### `pcomp`

`pcomp` allows for function style composition of properties.


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

;; 2-ary form for predicate check
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

### Properties 

`idx` indexes elements against 'properties', which are objects implementing the Property protocol. By default
an object used as a property is looked up as a key in the element. Which works well for the common use case of querying by key.

Functions implement Property, they are not looked up as keys, but rather applied to the element. 

#### Built-in Properties

- functions
- nested paths with `(path prop1 prop2 ...)`.
- any other object is looked up with `(get element o)`
- escape functions to `(get element o)` with `(as-key o)`

### Predicates

As well as supplying properties and values directly to `group` and `identify` you can omit the value and instead apply a Predicate.

A predicate is an object implementing the Predicate protocol. By default, objects applied as predicates are used as a property, whose truthyness is indexed.
This works well for existence checks, and function predicates. Therefore you can say `(group coll even?)`

#### Built-in Predicates

- compose predicates into a match with `(match prop1 val prop2 val)`, this can be used to form composite indexes over many properties.
- any other object is a truthyness test by applying at as a property.

## License

Copyright © 2020 Daniel Stone

Distributed under the [MIT](https://opensource.org/licenses/MIT) license.