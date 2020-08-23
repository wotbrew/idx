# idx

_`idx` lets you treat your collection like a database_.

This library provides clojure(script) data structures (maps, sets and vectors) that allow for secondary indexes. Providing 
alternative fast access paths to their elements.

Supports both Clojure and ClojureScript.

## Features

- Wrappers for vectors, sets and maps.
- Index elements on demand by any property, such as clojure functions, keywords, paths, or composites thereof. See [reference](#properties)
- Can choose automatic indexing, where indexes are created and cached transparently as you query the collection.
- Indexes are maintained incrementally as you modify your collection with functions - `conj`, `assoc` and so on.
- Query functions also work on normal collections so you can 'upgrade' them with indexes when you profile and find where you need them.
- Good for re-frame, add indexes without changing the shape of your data.

## Caveats

- If you are only querying once or twice, it is almost always faster to just use sequence functions than build indexes.
- For small n indexes are very expensive. Use it to flatten quadratic joins, do not use it to replace all sequence filtering.
- If you index by function, that function must absolutely be pure, otherwise all bets are off. Similar to comparators and (sorted-set-by).
- Each index uses memory, so we need to make sure we consider that. This is particularly important to think about when using automatic-indexing.
- When indexing by function, index identity is function identity - so you must be careful with lambdas and closures.

## Why 

It is common to have the problem of taking repeated linear lookups (as you might accomplish with `filter`) to a sub-linear one. Typically what happens
is you use `group-by` or write some code to transform your collection into some kind of map to allow for fast lookup of its elements.

There are 3 problems that `idx` tries to solve:

- proliferation of `-by-this` or `by-that` type locals (or worse args, or keys) that only serve as fast paths to your actual data.
- allowing for profiler driven optimisation without massively restructuring the code.
- index invalidation as data changes

Lets say we have a large collection of orders, and a large collection of items, I want to join the groups of items for each order under the :items key.

Typical solution would look like this:
```clojure
(let [item-idx (group-by :order-id items)]
  (for [order orders]
    (assoc order :items (item-idx (:order-id order)))))
```

Now this case is not too egregious, however in real code it is tempting to pass your indexes between functions, introducing accidental complexity to their signatures (the args would not be there if it were not for insufficient data structures).
If you do not do that - you are often loosing gains by not sharing them as you repeatedly construct expensive indexes.

Furthermore we often have to structure our code around the indexes, they are not easy to add and remove independent of the usages.

If you are manually creating indexes and have to change your data, then you have to do that yourself. 

There are many solutions in this space, but I have not seen one that does not convert your data into something else. You can look at this library
as a drop-in you can use to supercharge your existing collections.

`idx` is not trying to compete with databases like [datascript](https://github.com/tonsky/datascript). It is intending to compete with a proliferation of manual `group-by`, `index-by` style calls in order 
to find data in your collections.
 
The performance of course is not as good as manual indexing, but should be good enough for most of the same use cases.

## Usage

All functions are available in the `com.wotbrew.idx` namespace.

### Manually index your collection

```clojure
; (index coll p kind ...)
;; e.g 
(def coll [{:id 1, :name "fred", :created-at #inst "2018-03-14"} ...])
(index coll :id :idx/unique :name :idx/hash :created-at :idx/sort)
;=> 
[{:id 1, :name "fred", :created-at #inst "2018-03-14"} ...]
```

`index` returns a new indexed collection with the specified indexes (plus any that already existed).

You can also later remove indexes with `delete-index`.

Kind is either:

- `:idx/hash` for fast `lookup` calls
- `:idx/unique` for fast `identify` / `replace-by` calls.
- `:idx/sort` for fast `ascending` / `descending` calls.

The resulting indexing collection meets the interface of its (map/vector/set) input, if you add/associate/remove elements
the indexes will be maintained on your behalf.

### Automatically index your collection as it is queried.

```clojure 
(auto coll)
```

This returns an indexed collection that creates indexes (and caches them) on demand as you query.

This can be good in local contexts where you only want to specify indexes once where they are used.

Caveats to this approach:

- You could end up creating a lot of indexes, which slow down `conj`, `assoc`, `dissoc` and so on.
- Redundant indexes take up memory, remember you can `delete-index`.

You can still call `index` on `auto` collections if you want to force certain indexes ahead of time.

### Query your collection 

Once wrapped with `index` or `auto`, a small suite of functions is available to query your collection.

#### `lookup`

Uses a one-to-many hash index if available.

Returns sequences of (unsorted) elements. You pass a predicate or property to test and value to match.

Say you have a vectorß of users, each with an age key, you might do:

```clojure 
(lookup users :age 10)
```

Say you have a vector of numbers, and you want to find the negative ones, functions are both properties and predicates.

```clojure 
(def numbers [-1,3,4,-5])
(lookup numbers neg? true)
;; => 
[-5, -1]
```

#### `identify`

Uses a one-to-one hash index if available.

`identify` operates on unique properties and predicates. It will throw 
if the property is non unique. Good for by-id type queries.

```clojure 
(def users [{:id 32344, :name "Fred"} ...])
(identify users :id 32344)
;; => 
{:id 32344, :name "Fred"}
```

#### `ascending`, `descending`

Uses a one-to-many sorted index if available.

```clojure
(def users [{:name "Alice", :age 42}
            {:name "Bob", :age 30}
            {:name "Barbara", :age 12}
            {:name "Jim", :age 83}])

(ascending users :age > 30) ;; ascending sort
(descending users :age <= 30) ;; descending sort
```

#### `path`

`path` allows nested indexes, use it for nested indexes as if you
are indexing a get-in call.

```clojure
(lookup orders (path :user :id) 32344)
```

#### `match`

`match` composes properties to form composite indexes. `match` returns a Predicate implementation so
you do not have to redundantly respecify the structure in the value position.

```clojure
(lookup numbers (match neg? true, even? true))
```

They keys can be any property, and match nests.

This allows for some pretty extensive (and expensive!) indexes, but is useful to compose 
a couple of properties together for composite keys.

```clojure
(lookup orders (match (path :user :id) 32444,
                      :carrier (match :country "uk" :available true)
                      :delivery-date #"2020-08-17"))
```

In order to index a match, either use an `auto` coll or use the `:idx/value` placeholder.

```clojure
(index coll (match (path :user :id) :idx/value,
                   :carrier (match :country :idx/value :available :idx/value)
                   :delivery-date :idx/value))
```

#### `pred`

`pred` creates a predicate that uses a truthy/falsey index. It can be used in the value position of matches.

```clojure
(match :foo (pred :has-bar))
```

pred is also useful to promote a function so it can be used
in the predicate position of `lookup` / `identify` / `replace-by`.

```clojure
(lookup :foo (pred even?))
```

#### `pk`

`pk` looks up the (primary) index or key of the matched element.
 
Uses a unique one-to-one hash index if one is available.

```clojure 
(pk [1,4,5,3] identity 5) ;; => 2
(pk {:foo 42, :bar 33} identity 33) ;; => bar
(pk [{:foo 42}, {:foo 33}] :foo 42) ;; => 0
```

#### `pcomp`

`pcomp` allows for function style composition of properties, takes 2 properties and returns a property.

The property returned by `(pcomp a b)`

Will be looked each property in turn. `(a (b element))`

### Modify your collection

Any indexes (automatic or otherwise) against your collection will be maintained incrementally with new versions of the data structure.

Use normal clojure collection functions such as `conj`, `assoc`, `dissoc`, `disj` and `into`. 

Some handy functions are enabled due to the presence of indexes.

#### `replace-by`

Replaces an element in the collection identified by the property/value or predicate.

Uses a unique index if one is available (always true if you use [auto](#automatically-index-your-collection-as-it-is-queried))

e.g 

```clojure 
(replace-by customers :email "foo@bar.com" new-customer)
```

#### `unwrap`

As maintaining indexes can become expensive if you want to make a lot of modifications, you can completely remove any indexing
with `unwrap`.

```clojure
(unwrap coll)
```

#### `delete-index`

Returns a new collection without the specified index(es), uses same index specification as [index](#manually-index-your-collection)

```clojure 
(delete-index coll :foo :idx/hash)
```

## Reference

### Properties 

`idx` indexes elements against 'properties', which are objects implementing the Property protocol. By default
an object used as a property is looked up as a key in the element. Which works well for the common use case of querying by key.

Functions implement Property, they are not looked up as keys, but rather applied to the element. 

There are a couple of useful property combinators [match](#match) and [pcomp](#pcomp).

#### Built-in Properties

- functions
- nested paths with `(path prop1 prop2 ...)`.
- any other object is looked up with `(get element o)`
- escape functions to `(get element o)` with `(as-key o)`

#### Predicates 

Several functions take predicates as arguments, a predicate composes a property with its expected value. This is how `match` works.

You can lift any function into a truthy/falsey test with [pred](#pred).
 
#### Mutability 

- If you use [auto](#automatically-index-your-collection-as-it-is-queried) indexes are cached as you query using mutable fields. This is similar to how 
  clojure caches hash codes. This should have no impact on thinking of these structures as persistent as the only 
  characteristic it changes is the performance of repeated lookups. 
- When you query an [auto](#automatically-index-your-collection-as-it-is-queried) coll on multiple threads they could race as they try to create the index. This does not introduce
  consistency issues (as they will all return the same answer), it might cause some redundant computation however. If this is important, manually index with
  [index](#manually-index-your-collection).

### Benchmarks / Performance

All taken on a 2015 MBP using `bench` from [criterium](https://github.com/hugoduncan/criterium).

The performance goal is to get close enough to manual indexing speed using clojure data structures that it is a viable solution for most normal line-of-business code.

#### Index creation

Creating a :idx/hash index, indexing #(mod % 10) over 10000 integers.

```
Execution time mean : 4.079791 ms
```                   

For comparison `group-by`

```
Execution time mean : 1.973795 ms
```

Creating a :idx/unique value index on 10000 integers.

```
Execution time mean : 1.314164 ms
```

Doing the same manually `(persistent! (reduce-kv #(assoc! %1 %3 %2) (transient {}) sample))`

```
Execution time mean : 1.450698 ms
```

Which is unsurprising as the code does identical work.

Creating a :idx/sort index is the most expensive, as shown in this benchmark for the same collection and property.

``` 
Execution time mean : 5.673653 ms
```

#### Lookups 

Lookups are relatively cheap in all cases when indexes exist. When a scan is necessary performance is close or the same as to (filter). 

Lookups are somewhat slower than against equivalent maps, mostly because this library needs to contend with incremental index maintenance as collections change.

Looking up against a :idx/hash index.

```
Execution time mean : 118.559471 ns
```

For comparison `get` against the equivalent (group-by) map, literally just a map lookup.

```
Execution time mean : 43.276000 ns
```

Looking up against a :idx/unique index with identify.

```
Execution time mean : 127.409999 ns
```

For comparison getting from the equiv map:
```
Execution time mean : 51.197166 ns
```

#### Modification

Modification of indexed collections is difficult to benchmark, it depends on the number (and kind) of indexes, the properties you are testing (if functions).

It will almost always be the case that maintaining an index is more expensive than maintaining the original collection. In the case of vectors, the difference is stark, as vector writes are much cheaper
than the map writes that indexes require.

Conj onto 10000 integer vector with a unique value index

```
Execution time mean : 307.481521 ns
```

For comparison, conj + manual indexing in an equivalent value to element map.

``` 
Execution time mean : 188.853768 ns
```

### Future work 

- Could we expose specialised indexes, e.g kd, radix, b+.

## License

Copyright © 2020 Daniel Stone

Distributed under the [MIT](https://opensource.org/licenses/MIT) license.