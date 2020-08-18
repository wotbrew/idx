# idx

_`idx` lets you treat your collection like a database_.

This library provides clojure data structures (maps, sets and vectors) that allow for secondary indexes. Providing 
alternative fast access paths to their elements.

## Features

- Wrappers for vectors, sets and maps.
- Index elements on demand by any property, such as clojure functions, keywords, paths, selections. See [reference](#properties)
- Can choose automatic indexing, where indexes are created and cached transparently as you query the collection.
- Indexes are maintained incrementally as you modify your collection with functions - `conj`, `assoc` and so on.
- Query functions also work on normal collections so you can 'upgrade' them with indexes when you profile and find where you need them.

## Caveats

- No ClojureScript support, Clojure only for now. (pull requests welcome!)
- If you are only querying once or twice, it is almost always faster to just use sequence functions than build indexes.
- For small n indexes are very expensive. Use it to flatten quadratic joins, do not use it to replace all sequence filtering.
- If you index by function, that function must absolutely be pure, otherwise all bets are off. Similar to comparators and (sorted-set-by).
- Each index uses memory, so we need to make sure we consider that. This is particularly important to think about when using automatic-indexing.

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
    (assoc order :items (item-idx (:order-id order))))  )
```

Now this case is not too egregious, however in real code it is tempting to pass your indexes between functions, introducing incidental complexity to their signatures, which I find very, very smelly.
If you do not do that - you are often loosing gains by not sharing them as you repeatedly construct expensive indexes.

Furthermore we have to structure our code around the indexes, they are not easy to add and remove independent of the usages.

If you are manually creating indexes and have to change your data, then you have to do that yourself. 

There are many solutions in this space, but I have not seen one that does not convert your data into something else. You can look at this library
as a drop-in you can use to supercharge your existing collections.

`idx` is not trying to compete with databases like [datascript](https://github.com/tonsky/datascript). It is intending to compete with a proliferation of manual `group-by`, `index-by` style calls in order 
to find data in your collections.
 
 **Its performance should be good enough that it competes with manual indexing**.

## Usage

All functions are available in the `com.wotbrew.idx` namespace.

### Manually index your collection

```clojure
(idx coll p kind ...)
;; e.g 
(idx coll :id :idx/unique :name :idx/hash :created-at :idx/sort)
```

`idx` returns a new indexed collection with the specified indexes (plus any that already existed).

You can also later remove indexes with `delete-index`.

Kind is either:

- `:idx/hash` for fast `group` calls
- `:idx/unique` for fast `identify` / `replace-by` calls.
- `:idx/sorted` for fast `ascending` / `descending` calls.

The resulting indexing collection meets the interface of its (map/vector/set) input, if you add/associate/remove elements
the indexes will be maintained on your behalf.

### Automatically index your collection as it is queried.

```clojure 
(auto-idx coll)
```

This returns an indexed collection that creates indexes (and caches them) on demand as you query.

This can be good in local contexts where you only want to specify indexes once where they are used.

Caveats to this approach:

- You could end up creating a lot of indexes, which slow down `conj`, `assoc`, `dissoc` and so on.
- Redundant indexes take up memory, remember you can `delete-index`.

You can still call `idx` on `auto-idx` collections if you want to force certain indexes ahead of time.

### Query your collection 

Once wrapped with idx, a small suite of functions is available to query your collection.

#### `group`

Uses a one-to-many hash index if available.

Returns vectors of (unsorted) elements. You pass a predicate or property to test and value to match.

Say you have a vector of users, each with an age key, you might do:

```clojure 
(group users :age 10)
```

Say you have a vector of numbers, and you want to find the negative ones, functions are both properties and predicates.

```clojure 
(group numbers neg? true)
;; or
(group coll neg?)
```

#### `identify`

Uses a one-to-one hash index if available.

`identify` operates on unique properties and predicates. It will throw 
if the property is non unique. Good for by-id type queries.

```clojure 
(identify users :id 32344)
```

#### `ascending`, `descending`

Uses a one-to-many sorted index if available.

```clojure
(ascending users :created-at > #inst "2020-08-18") ;; ascending sort
(descending users :created-at <= #inst "2020-08-18") ;; descending sort
```

#### `path`

`path` allows nested indexes, use it for nested indexes as if you
are indexing a get-in call.

```clojure
(group orders (path :user :id) 32344)
```

#### `match`

`match` composes properties to form composite indexes. `match` returns a Predicate implementation so
you do not have to redundantly respecify the structure in the value position.

```clojure
(group numbers (match neg? true, even? true))
```

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

`pred` creates a predicate that uses a truthy/falsey index. It can be used in the value position of matches.

```clojure
(match :foo (pred even?))
```

pred is also useful to promote a function so it can be used
in the predicate position of `group` / `identify` / `replace-by`.

```clojure
(group :foo (pred even?))
```

#### `pcomp`

`pcomp` allows for function style composition of properties.

### Modify your collection

Any indexes (automatic or otherwise) against your collection will be maintained incrementally with new versions of the data structure.

Use normal clojure collection functions such as `conj`, `assoc`, `dissoc`, `disj` and `into`. 

Some handy functions are enabled due to the presence of indexes.

#### `replace-by`

Replaces an element in the collection identified by the property/value or predicate.

Uses a unique index if one is available (always true if you [auto-idx](#automatically-index-your-collection-as-it-is-queried))

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

Returns a new collection without the specified index(es), uses same index specification as [idx](#manually-index-your-collection)

```clojure 
(delete-index coll :foo :idx/hash)
```

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

#### Predicates 

Several functions take predicates as arguments, a predicate composes a property with its expected value. This is how `match` works.

You can lift any function into a truthy/falsey test with [pred](#pred).
 
#### Mutability 

- If you use [auto-idx](#automatically-index-your-collection-as-it-is-queried) indexes are cached as you query using mutable fields. This is similar to how 
  clojure caches hash codes. This should have no impact on thinking of these structures as persistent as the only 
  characteristic it changes is the performance of repeated lookups. 
- When you query an [auto-idx](#automatically-index-your-collection-as-it-is-queried) coll on multiple threads they could race as they try to create the index. This does not introduce
  consistency issues (as they will all return the same answer), it might cause some redundant computation however. If this is important, manually index with
  [idx](#manually-index-your-collection).
 

## License

Copyright © 2020 Daniel Stone

Distributed under the [MIT](https://opensource.org/licenses/MIT) license.