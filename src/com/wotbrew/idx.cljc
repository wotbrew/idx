(ns com.wotbrew.idx
  (:require [com.wotbrew.impl.ext]
            [com.wotbrew.impl.protocols :as p]
            [com.wotbrew.impl.index :as i])
  (:import #?@(:clj [(clojure.lang LazilyPersistentVector)])))

#?(:clj (set! *warn-on-reflection* true))

(defn auto-idx
  "Takes a set, vector or map and wraps it so that its elements support indexed queries.

  Indexes are created on demand to satisfy queries and then are reused.

  Indexes once realised will be maintained incrementally as you call conj, assoc and so on on the collection.

  The coll must be a vector, map or set. If you pass a seq/seqable/iterable it is converted to a vector.

  Metadata is carried over to the new structure.

  If the collection is already indexed, it is returned as-is without modification."
  [coll]
  (p/-wrap coll true))

(defn unwrap
  "Returns the backing collection without indexes."
  [coll]
  (p/-unwrap coll))

(defn idx
  "Adds indexes to the collection, returning a new indexed collection.

  Specify a property/predicate to index, and a `kind` being:

  `:idx/unique` (for identify and replace-by calls)
  `:idx/hash (for group calls)`
  `:idx/sort` (for ascending/descending calls)"
  ([coll] coll)
  ([coll p kind] (p/-add-index coll p kind))
  ([coll p kind & more]
   (loop [coll (p/-add-index coll p kind)
          more more]
     (if-some [[p kind & more] (seq more)]
       (recur (p/-add-index coll p kind) more)
       coll))))

(defn delete-index
  "Deletes indexes from the collection, returning a new collection."
  ([coll p kind] (p/-del-index coll p kind))
  ([coll p kind & more]
   (loop [coll (p/-del-index coll p kind)
          more more]
     (if-some [[p kind & more] (seq more)]
       (recur (p/-del-index coll p kind) more)
       coll))))

(defrecord Comp [p1 p2]
  p/Property
  (-property [this element] (p/-property p1 (p/-property p2 element))))

(defn pcomp
  "Like clojure.core/comp but on properties."
  [p1 p2]
  (->Comp p1 p2))

(defrecord Select [ps]
  p/Property
  (-property [this element]
    (reduce (fn [m p] (assoc m p (p/-property p element))) {} ps)))

(defrecord Pred [p v]
  p/Predicate
  (-prop [this] p)
  (-val [this] v))

(defn pred
  "Can be used in matches to nest truthy/falsey predicates.

  When you want to test truthyness (pred p) or (pred p true), if you want to test falseyness (pred p false)."
  ([p] (->Pred (pcomp boolean p) true))
  ([p v] (->Pred (pcomp boolean p) v)))

(defn- build-match-map
  [m p v]
  (if (instance? Pred v)
    (build-match-map m (pcomp (p/-prop v) p) (p/-val v))
    (assoc m p v)))

(defn match
  "Takes a map of {property value}, if each property value pair matches, the element
  is returned. If value is itself a match then it will be nested."
  [p v & more]
  (let [m (loop [m (build-match-map {} p v)
                 more more]
            (if-some [[prop val & tail] (seq more)]
              (recur (build-match-map m prop val) tail)
              m))]
    (->Pred (->Select (set (keys m))) m)))

(defrecord AsKey [k]
  p/Property
  (-property [this element] (get element k)))

(defn as-key
  "Returns a property that looks up k as a key. Only useful if you are using functions as keys."
  [k]
  (->AsKey k))

(defrecord Path2 [a b]
  p/Property
  (-property [this element] (->> element (p/-property a) (p/-property b))))

(defrecord Path3 [a b c]
  p/Property
  (-property [this element] (->> element (p/-property a) (p/-property b) (p/-property c))))

(defrecord Path [ps]
  p/Property
  (-property [this element]
    (reduce
      (fn [v p] (p/-property p v))
      element
      ps)))

(defn path
  "Returns a property that will drill down to some nested value by using the properties in the list. Think get-in."
  ([p] p)
  ([p1 p2] (->Path2 p1 p2))
  ([p1 p2 p3] (->Path3 p1 p2 p3))
  ([p1 p2 p3 p4 & more] (->Path (reduce conj [p1 p2 p3 p4] more))))

(defn group
  "Returns an (unordered) vector of items where (p element) equals v.

  The 2-ary takes a **predicate** which composes a property with its expected value, either a `(match)` form, or a `(pred)` form."
  ([coll pred] (group coll (p/-prop pred) (p/-val pred)))
  ([coll p v]
   (if (instance? Pred v)
     (group coll (pcomp (p/-prop v) p) (p/-val v))
     (if-some [i (p/-get-index coll p :idx/hash)]
       (let [m (get i v {})]
         (if (<= (count m) 32)
           #?(:clj
              (let [a (object-array (count m))]
                (reduce-kv (fn [i id _] (aset a (int i) (coll id)) (unchecked-inc-int i)) (int 0) m)
                (LazilyPersistentVector/createOwning a))
              :cljs
              (mapv coll (vals m)))
           (persistent! (transduce (map coll) conj! (transient []) (vals m)))))
       (filterv (fn [element] (= v (p/-property p element))) (p/-elements coll))))))

(defn pk-group
  "Like group but returns you the (index/keys) of the elements rather than the elements themselves"
  ([coll pred] (pk-group coll (p/-prop pred) (p/-val pred)))
  ([coll p v]
   (if (instance? Pred v)
     (pk-group coll (pcomp (p/-prop v) p) (p/-val p))
     (if-some [i (p/-get-index coll p :idx/hash)]
       (let [m (get i v {})]
         (vec (vals m)))
       (map first (filter (fn [[_ element]] (= v (p/-property p element))) (p/-id-element-pairs coll)))))))

(defn identify
  "Returns the unique element where the property equals v."
  ([coll pred] (identify coll (p/-prop pred) (p/-val pred)))
  ([coll p v]
   (if (instance? Pred v)
     (identify coll (pcomp (p/-prop v) p) (p/-val v))
     (if-some [i (p/-get-index coll p :idx/unique)]
       (let [id (get i v)]
         (when (some? id)
           (coll id)))
       (some (fn [element] (when (= v (p/-property p element)) element)) (p/-elements coll))))))

(defn pk
  "Returns the key (index/map key) given a unique property/value pair or predicate."
  ([coll pred] (pk coll (p/-prop pred) (p/-val pred)))
  ([coll p v]
   (if (instance? Pred v)
     (identify coll (pcomp (p/-prop v) p) (p/-val v))
     (if-some [i (p/-get-index coll p :idx/unique)]
       (get i v)
       (some (fn [[id element]] (when (= v (p/-property p element)) id)) (p/-id-element-pairs coll))))))

(defn replace-by
  "Replaces an element by an alternative key."
  ([coll pred element] (replace-by coll (p/-prop pred) (p/-val pred) element))
  ([coll p v element]
   (if (instance? Pred v)
     (replace-by coll (pcomp (p/-prop v) p) (p/-val v) element)
     (if-some [i (p/-get-index coll p :idx/unique)]
       (let [id (get i v)]
         (if (associative? coll)
           (assoc coll id element)
           (-> coll (disj id) (conj element))))
       (let [id (some (fn [[id element]] (when (= v (p/-property p element)) id)) (p/-id-element-pairs coll))]
         (if (associative? coll)
           (assoc coll id element)
           (-> coll (disj id) (conj element))))))))

(defn ascending
  "Returns an ascending order seq of elements where (test (p element) v) returns true.

  The order is defined by the value of v.

  This is much like subseq in the clojure.core."
  [coll p test v]
  (let [i (p/-get-index coll p :idx/sort)
        i (or i (i/create-sorted-from-elements (p/-elements coll) p))]
    (if (some? i)
      (->> (subseq i test v)
           (mapcat (fn [e] (map coll (vals (val e))))))
      ())))

(defn descending
  "Returns a descending order seq of elements where (test (p element) v) returns true.

  The order is defined by the value of v.

  This is much like rsubseq in the clojure.core."
  [coll p test v]
  (let [i (p/-get-index coll p :idx/sort)
        i (or i (i/create-sorted-from-elements (p/-elements coll) p))]
    (if (some? i)
      (->> (rsubseq i test v)
           (mapcat (fn [e] (map coll (vals (val e))))))
      ())))