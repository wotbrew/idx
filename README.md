# idx

This library provides clojure data structures (maps, sets and vectors) that allow for secondary indexes. Providing 
alternative fast access paths to their elements.

## Features

- Wrappers for vectors, sets and maps.
- Index elements on demand by any property, such as clojure functions, keywords, paths, selections. See [reference](#properties)
- Can choose automatic indexing, where indexes are created and cached transparently as you query the collection.
- Indexes are maintained incrementally through `conj`, `assoc` and so on.

## Caveats

- No ClojureScript support, Clojure only for now. (pull requests welcome!)
- If you are only querying once or twice, it is almost always faster to just use sequence functions than build indexes.
- For small n indexes are very expensive. Use it to flatten quadratic joins, do not use it to replace all sequence filtering.
- If you index by function, that function must absolutely be pure, otherwise all bets are off. Similar to comparators and (sorted-set-by).
- Each index use memory proportional to the collection size. 

## Usage

All functions are available in the `com.wotbrew.idx` namespace.

### Manually index your collection

```clojure
(idx coll)
;; or
(idx coll p kind ...)

;; e.g 

(idx coll :id :idx/unique :name :idx/hash :created-at :idx/sort)
```

`idx` returns a new indexed collection with the specified indexes (plus any that already existed).

You can also later remove indexes with `delete-index`.

Kind is either:

`:idx/hash` for fast `group` calls
`:idx/unique` for fast `identify` / `replace-by` calls.
`:idx/sorted` for fast `ascending` / `descending` calls.

The resulting indexing collection meets the interface of its (map/vector/set) input, if you add/associate/remove elements
the indexes will be maintained on your behalf.

### Automatically index your collection as it is queried.

```clojure 
(auto-idx coll)
```

This returns an indexed collection that creates indexes (and caches) them on demand as you query.

This can be good in local contexts where you only want to specify indexes once where they are used.

Caveats to this approach:

- You could end up creating a lot of indexes, which slow down `conj`, `assoc`, `dissoc` and so on.
- Redundant indexes take up memory, remember you can `delete-index`.

### Query your collection 

Once wrapped with idx, a small suite of functions is available to query your collection.

#### `group`

Uses a one-to-many hash index.

Returns vectors of elements. You pass a predicate or property to test and value to match.

Say you have a vector of users, each with an age key, you might do:

`(group users :age 10)` 

Say you have a vector of numbers, and you want to find the negative ones, functions are both properties and predicates.

`(group numbers neg? true)` or `(group coll neg?)`.

#### `identify`

Uses a one-to-one hash index

`identify` operates on unique properties and predicates. It will throw 
if the property is non unique. Good for by-id type queries.

`(identify users :id 32344)`

#### `ascending`, `descending`

Uses a one-to-many sorted index.

#### `path`

`path` allows nested indexes, use it for nested indexes as if you
are indexing a get-in call.

`(group orders (path :user :id) 32344)`

#### `match`

`match` composes properties to form composite indexes. `match` returns a Predicate implementation so
you do not have to redundantly respecify the structure in the value position.

`(group numbers (match neg? true, even? true))`

They keys can be any property, and match nests.

This allows for some pretty extensive (and expensive!) indexes, but is useful to compose 
a couple of properties together for joins.

```clojure
(group orders (match (path :user :id) 32444,
                     :carrier (match :country "uk" :available true)
                     :delivery-date #"2020-08-17"))
```

In order to index a match, either use an `auto-idx` coll or use the `:idx/value` placeholder.

```clojure
(idx coll (match (path :user :id) :idx/value,
                 :carrier (match :country :idx/value :available :idx/value)
                 :delivery-date :idx/value))
```

#### `pred`

`pred` allows you to place truthyness/falseyness tests in the value position(s) of `match`, `group` and `identify` calls.

`(match :foo (pred even?))`

#### `pcomp`

`pcomp` allows for function style composition of properties.

### Modify your collection

Any indexes (automatic or otherwise) against your collection will be maintained incrementally with new versions of the data structure.

Use normal clojure collection functions such as `conj`, `assoc`, `dissoc`, `disj` and `into`. 

Some handy functions are enabled due to the presence of indexes.

#### `replace-by`

Replaces an element in the collection identified by the property/value or predicate.

e.g 

```clojure 
(replace-by customers :email "foo@bar.com" new-customer)
```

#### `unwrap`

As maintaining indexes can become expensive if you want to make a lot of modifications, you can completely remove any indexing
with `(unwrap coll)`.

#### `delete-index`

Returns a new collection without the specified index(es).

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

## License

Copyright © 2020 Daniel Stone

Distributed under the [MIT](https://opensource.org/licenses/MIT) license.