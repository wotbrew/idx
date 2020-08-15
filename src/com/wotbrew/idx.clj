(ns com.wotbrew.idx
  (:require [clojure.walk :as walk])
  (:import (clojure.lang IPersistentMap Associative ILookup IPersistentCollection Seqable Counted MapEquivalence IHashEq IFn IMeta IObj ArityException IPersistentVector IPersistentSet IPersistentStack Indexed Reversible Sequential Keyword Var Fn LazilyPersistentVector)
           (java.util Map$Entry Map List Set Collection RandomAccess ArrayList)))

(set! *warn-on-reflection* true)

(defprotocol Property
  (-property [this element]))

(defprotocol Idx
  "You should consider this protocol an implementation detail for now."
  (-get-uniq [idx p])
  (-get-eq [idx p])
  (-get-sorted [idx p]))

;; for eq/sorted leaves we use maps rather than sets so
;; we get the PersistentArrayMap optimisation when small and faster reduce.

(defn- add-eq
  ([eq id element]
   (reduce-kv
     (fn [eq p i]
       (let [v (-property p element)
             nset (get i v {})
             nset (assoc nset id id)
             i (assoc i v nset)]
         (assoc eq p i)))
     eq
     eq))
  ([eq id old-element element]
   (reduce-kv
     (fn [eq p i]
       (let [ov (-property p old-element)
             v (-property p element)]
         (if (identical? ov v)
           eq
           (let [oset (get i ov {})
                 oset (dissoc oset id)
                 i (if (empty? oset) (dissoc i ov) (assoc i ov oset))

                 nset (get i v {})
                 nset (assoc nset id id)
                 i (assoc i v nset)]
             (assoc eq p i)))))
     eq
     eq)))

(defn- del-eq [eq id old-element]
  (reduce-kv
    (fn [eq p i]
      (let [ov (-property p old-element)]
        (let [oset (get i ov {})
              oset (dissoc oset id)
              i (if (empty? oset) (dissoc i ov) (assoc i ov oset))]
          (if (empty? i)
            (dissoc eq p)
            (assoc eq p i)))))
    eq
    eq))

(defn- add-uniq
  ([unq id element]
   (reduce-kv
     (fn [unq p i]
       (let [v (-property p element)
             i (assoc i v id)]
         (assoc unq p i)))
     unq
     unq))
  ([unq id old-element element]
   (reduce-kv
     (fn [unq p i]
       (let [ov (-property p old-element)
             v (-property p element)]
         (if (identical? ov v)
           unq
           (let [i (dissoc i ov)
                 i (assoc i v id)]
             (assoc unq p i)))))
     unq
     unq)))

(defn- del-uniq [unq old-element]
  (reduce-kv
    (fn [unq p i]
      (let [ov (-property p old-element)]
        (let [i (dissoc i ov)]
          (if (empty? i)
            (dissoc unq p)
            (assoc unq p i)))))
    unq
    unq))

(defn- add-sorted
  ([srt id element]
   (reduce-kv
     (fn [srt p i]
       (let [v (-property p element)
             nset (get i v {})
             nset (assoc nset id id)
             i (assoc i v nset)]
         (assoc srt p i)))
     srt
     srt))
  ([srt id old-element element]
   (reduce-kv
     (fn [srt p i]
       (let [ov (-property p old-element)
             v (-property p element)]
         (if (identical? ov v)
           srt
           (let [oset (get i ov {})
                 oset (dissoc oset id)
                 i (if (empty? oset) (dissoc i ov) (assoc i ov oset))

                 nset (get i v {})
                 nset (assoc nset id id)
                 i (assoc i v nset)]
             (assoc srt p i)))))
     srt
     srt)))

(defn- del-sorted [srt id old-element]
  (reduce-kv
    (fn [srt p i]
      (let [ov (-property p old-element)]
        (let [oset (get i ov {})
              oset (dissoc oset id)
              i (if (empty? oset) (dissoc i ov) (assoc i ov oset))]
          (if (empty? i)
            (dissoc srt p)
            (assoc srt p i)))))
    srt
    srt))

(deftype IndexedPersistentMap
  [m
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted]
  Idx
  (-get-eq [idx p]
    (or (get eq p)
        (let [rf (fn [m id v]
                   (let [ival (-property p v)]
                     (assoc! m ival (assoc (get m ival {}) id id))))
              i (persistent! (reduce-kv rf (transient {}) m))
              eq (assoc eq p i)]
          (set! (.-eq idx) eq)
          i)))
  (-get-uniq [idx p]
    (or (get uniq p)
        (let [rf (fn [m id v]
                   (let [ival (-property p v)]
                     (assert (not (contains? m ival)))
                     (assoc! m ival id)))
              i (persistent! (reduce-kv rf (transient {}) m))
              uniq (assoc uniq p i)]
          (set! (.-uniq idx) uniq)
          i)))
  (-get-sorted [idx p]
    (or (get sorted p)
        (let [rf (fn [m id v]
                   (let [ival (-property p v)]
                     (assoc m ival (assoc (get m ival {}) id id))))
              i (reduce-kv rf (sorted-map) m)
              srt (assoc sorted p i)]
          (set! (.-sorted idx) srt)
          i)))

  Map
  (size [this] (.size ^Map m))
  (isEmpty [this] (.isEmpty ^Map m))
  (containsValue [this v] (.containsValue ^Map m v))
  (get [this k] (.get ^Map m k))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.)))
  (putAll [this m2] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (.keySet ^Map m))
  (values [this] (.values ^Map m))
  (entrySet [this] (.entrySet ^Map m))

  IMeta
  (meta [this] (.meta ^IMeta m))
  IObj
  (withMeta [this mta] (IndexedPersistentMap. (.withMeta ^IObj m mta) eq uniq sorted))

  MapEquivalence
  IHashEq
  (hasheq [this] (.hasheq ^IHashEq m))

  IFn
  (invoke [this o] (.invoke ^IFn m o))
  (invoke [this o o1] (.invoke ^IFn m o o1))
  (applyTo [this arglist] (apply m arglist))

  IPersistentMap
  (assoc [this o o1]
    (let [old-element (get this o ::not-found)]
      (cond
        (identical? o1 old-element) this
        (identical? old-element ::not-found)
        (IndexedPersistentMap.
          (.assoc ^IPersistentMap m o o1)
          (some-> eq (add-eq o o1))
          (some-> uniq (add-uniq o o1))
          (some-> sorted (add-sorted o o1)))

        :else
        (IndexedPersistentMap.
          (.assoc ^IPersistentMap m o o1)
          (some-> eq (add-eq o old-element o1))
          (some-> uniq (add-uniq o old-element o1))
          (some-> sorted (add-sorted o old-element o1))))))
  (assocEx [this o o1]
    (if (contains? m o)
      (throw (Exception. "Key already present"))
      (assoc this o o1)))
  (without [this o]
    (let [old-element (get this o ::not-found)]
      (if (identical? ::not-found old-element)
        this
        (IndexedPersistentMap.
          (.without ^IPersistentMap m o)
          (some-> eq (del-eq o old-element))
          (some-> uniq (del-uniq old-element))
          (some-> sorted (del-sorted o old-element))))))
  Counted
  Iterable
  (iterator [this] (.iterator ^Iterable m))
  Seqable
  (seq [this] (.seq ^Seqable m))
  IPersistentCollection
  (count [this] (.count ^IPersistentCollection m))
  (cons [^IPersistentMap this o]
    (cond
      (instance? Map$Entry o)
      (let [^Map$Entry e o]
        (.assoc this (.getKey e) (.getKey e)))
      (vector? o)
      (if (= 2 (count o))
        (throw (IllegalArgumentException. "Vector arg to map conj must be a pair"))
        (.assoc this (nth o 0) (nth o 1)))
      (map? o) (reduce-kv assoc this o)
      :else
      (reduce
        (fn [^IPersistentMap this ^Map$Entry e]
          (.assoc this (.getKey e) (.getKey e)))
        this
        o)))
  (empty [this] (IndexedPersistentMap. (.empty ^IPersistentCollection m) nil nil nil))
  (equiv [this o] (.equiv ^IPersistentCollection m o))
  ILookup
  (valAt [this o] (.valAt ^ILookup m o))
  (valAt [this o o1] (.valAt ^ILookup m o o1))
  Associative
  (containsKey [this o] (.containsKey ^Associative m o))
  (entryAt [this o] (.entryAt ^Associative m o))
  Object
  (equals [this obj] (.equals m obj))
  (hashCode [this] (.hashCode m))
  (toString [this] (.toString m))
  Callable
  (call [this] (throw (ArityException. 0 "IndexedPersistentMap")))
  Runnable
  (run [this] (throw (ArityException. 0 "IndexedPersistentMap"))))

(deftype IndexedPersistentVector
  [v
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted]
  Idx
  (-get-eq [idx p]
    (or (get eq p)
        (let [rf (fn [m id v]
                   (let [ival (-property p v)]
                     (assoc! m ival (assoc (get m ival {}) id id))))
              i (persistent! (reduce-kv rf (transient {}) v))
              eq (assoc eq p i)]
          (set! (.-eq idx) eq)
          i)))
  (-get-uniq [idx p]
    (or (get uniq p)
        (let [rf (fn [m id v]
                   (let [ival (-property p v)]
                     (assert (not (contains? m ival)))
                     (assoc! m ival id)))
              i (persistent! (reduce-kv rf (transient {}) v))
              uniq (assoc uniq p i)]
          (set! (.-uniq idx) uniq)
          i)))
  (-get-sorted [idx p]
    (or (get sorted p)
        (let [rf (fn [m id v]
                   (let [ival (-property p v)]
                     (assoc m ival (assoc (get m ival {}) id id))))
              i (reduce-kv rf (sorted-map) v)
              srt (assoc sorted p i)]
          (set! (.-sorted idx) srt)
          i)))
  RandomAccess
  Comparable
  (compareTo [this o] (.compareTo ^Comparable v o))
  IHashEq
  (hasheq [this] (.hasheq ^IHashEq v))

  List
  (size [this] (.size ^List v))
  (isEmpty [this] (.isEmpty ^List v))
  (contains [this o] (.contains ^List v o))
  (toArray [this] (.toArray ^List v))
  (toArray [this a] (.toArray ^List v a))
  (add [this o] (throw (UnsupportedOperationException.)))
  (^boolean remove [this ^Object o] (throw (UnsupportedOperationException.)))
  (^Object remove [this ^int o] (throw (UnsupportedOperationException.)))
  (containsAll [this coll] (.containsAll ^List v coll))
  (addAll [this coll] (throw (UnsupportedOperationException.)))
  (addAll [this i coll] (throw (UnsupportedOperationException.)))
  (removeAll [this coll] (throw (UnsupportedOperationException.)))
  (retainAll [this coll] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (get [this i] (.get ^List v i))
  (indexOf [this o] (.indexOf ^List v o))
  (lastIndexOf [this o] (.indexOf ^List v o))
  (listIterator [this] (.listIterator ^List v))
  (listIterator [this i] (.listIterator ^List v i))
  (subList [this from to] (.subList ^List v from to ))
  (set [this i o] (throw (UnsupportedOperationException.)))
  (add [this i o] (throw (UnsupportedOperationException.)))
  Iterable
  (iterator [this] (.iterator ^Iterable v))
  Counted
  IObj
  (withMeta [this meta] (IndexedPersistentVector. (.withMeta ^IObj v meta) eq uniq sorted))
  IMeta
  (meta [this] (.meta ^IMeta v))
  IPersistentVector
  (length [this] (.length ^IPersistentVector v))
  (assocN [this i val]
    (let [old-element (nth this i ::not-found)]
      (cond
        (identical? val old-element) this

        (identical? val ::not-found)
        (IndexedPersistentVector.
          (.assocN ^IPersistentVector v i val)
          (some-> eq (add-eq i val))
          (some-> uniq (add-uniq i val))
          (some-> sorted (add-sorted i val)))

        :else
        (IndexedPersistentVector.
          (.assocN ^IPersistentVector v i val)
          (some-> eq (add-eq i old-element val))
          (some-> uniq (add-uniq i old-element val))
          (some-> sorted (add-sorted i old-element val))))))
  (cons [this val]
    (let [i (.length ^IPersistentVector v)]
      (IndexedPersistentVector.
        (.cons ^IPersistentVector v val)
        (some-> eq (add-eq i val))
        (some-> uniq (add-uniq i val))
        (some-> sorted (add-sorted i val)))))
  Seqable
  (seq [this] (.seq ^Seqable v))
  Reversible
  (rseq [this] (.rseq ^Reversible v))
  Indexed
  (nth [this i] (.nth ^Indexed v i))
  (nth [this i notFound] (.nth ^Indexed v i notFound))
  IPersistentCollection
  (count [this] (.count ^IPersistentCollection v))
  (empty [this] (IndexedPersistentVector. (.empty ^IPersistentCollection v) eq uniq sorted))
  (equiv [this o] (.equiv ^IPersistentCollection v o))
  IPersistentStack
  (peek [this] (.peek ^IPersistentStack v))
  (pop [this]
    (let [i (.length ^IPersistentVector v)
          old-element (if (neg? i) nil (nth this (dec i)))]
      (IndexedPersistentVector.
        (pop ^IPersistentStack v)
        (some-> eq (del-eq i old-element))
        (some-> uniq (del-uniq old-element))
        (some-> sorted (del-sorted i old-element)))))
  ILookup
  (valAt [this key] (.valAt ^ILookup v key))
  (valAt [this key notFound] (.valAt ^ILookup v key notFound))
  Associative
  (containsKey [this key] (.containsKey ^Associative v key))
  (entryAt [this key] (.entryAt ^Associative v key))
  (assoc [this key val] (.assocN ^IPersistentVector this (int key) val))
  Object
  (hashCode [this] (.hashCode v))
  (equals [this obj] (.equals v obj))
  (toString [this] (.toString v))
  Sequential
  IFn
  (invoke [this arg1] (.invoke ^IFn v arg1))
  (invoke [this arg1 arg2] (.invoke ^IFn v arg1 arg2))
  (applyTo [this arglist] (apply v arglist))
  Callable
  (call [this] (throw (ArityException. 0 "IndexedPersistentVector")))
  Runnable
  (run [this] (throw (ArityException. 0 "IndexedPersistentVector"))))

(deftype IndexedPersistentSet
  [s
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted]
  Idx
  (-get-eq [idx p]
    (or (get eq p)
        (let [rf (fn [m id]
                   (let [ival (-property p id)]
                     (assoc! m ival (assoc (get m ival {}) id id))))
              i (persistent! (reduce rf (transient {}) s))
              eq (assoc eq p i)]
          (set! (.-eq idx) eq)
          i)))
  (-get-uniq [idx p]
    (or (get uniq p)
        (let [rf (fn [m id]
                   (let [ival (-property p id)]
                     (assert (not (contains? m ival)))
                     (assoc! m ival id)))
              i (persistent! (reduce rf (transient {}) s))
              uniq (assoc uniq p i)]
          (set! (.-uniq idx) uniq)
          i)))
  (-get-sorted [idx p]
    (or (get sorted p)
        (let [rf (fn [m id]
                   (let [ival (-property p id)]
                     (assoc m ival (assoc (get m ival {}) id id))))
              i (reduce rf (sorted-map) s)
              srt (assoc sorted p i)]
          (set! (.-sorted idx) srt)
          i)))

  IObj
  (withMeta [this meta] (IndexedPersistentSet. (.withMeta ^IObj s meta) eq uniq sorted))
  IMeta
  (meta [this] (.meta ^IMeta s))
  Collection
  IHashEq
  (hasheq [this] (.hasheq ^IHashEq s))
  IFn
  (invoke [this arg1] (.invoke ^IFn s arg1))
  (invoke [this arg1 arg2] (.invoke ^IFn s arg1 arg2))
  (applyTo [this arglist] (apply s arglist))
  Set
  (size [this] (.size ^Set s))
  (isEmpty [this] (.isEmpty ^Set s))
  (iterator [this] (.iterator ^Set s))
  (toArray [this] (.toArray ^Set s))
  (toArray [this a] (.toArray ^Set s a))
  (add [this e] (throw (UnsupportedOperationException.)))
  (remove [this o] (throw (UnsupportedOperationException.)))
  (containsAll [this p] (.containsAll ^Set s p))
  (addAll [this p] (throw (UnsupportedOperationException.)))
  (removeAll [this p] (throw (UnsupportedOperationException.)))
  (retainAll [this p] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  IPersistentSet
  (disjoin [this key]
    (let [ns (.disjoin ^IPersistentSet s key)]
      (if (identical? ns s)
        this
        (IndexedPersistentSet.
          ns
          (some-> eq (del-eq key key))
          (some-> uniq (del-uniq key))
          (some-> sorted (del-sorted key key))))))
  (contains [this key] (.contains ^IPersistentSet s key))
  (get [this key] (.get ^IPersistentSet s key))
  Seqable
  (seq [this] (.seq ^IPersistentSet s))
  IPersistentCollection
  (count [this] (.count ^IPersistentSet s))
  (cons [this o]
    (let [ns (.cons ^IPersistentSet s o)]
      (if (identical? ns s)
        this
        (IndexedPersistentSet.
          ns
          (some-> eq (add-eq o o))
          (some-> uniq (add-uniq o o))
          (some-> sorted (add-sorted o o))))))
  (empty [this] (IndexedPersistentSet. (.empty ^IPersistentSet s) nil nil nil))
  (equiv [this o] (.equiv ^IPersistentSet s o))
  Object
  (hashCode [this] (.hashCode s))
  (equals [this obj] (.equals s obj))
  (toString [this] (.toString s)))

(defprotocol Wrap
  (-wrap [coll]))

(extend-protocol Wrap
  IndexedPersistentMap (-wrap [this] this)
  IndexedPersistentVector (-wrap [this] this)
  IndexedPersistentSet (-wrap [this] this)
  IPersistentMap
  (-wrap [this] (->IndexedPersistentMap this nil nil nil))
  IPersistentVector
  (-wrap [this] (->IndexedPersistentVector this nil nil nil))
  IPersistentSet
  (-wrap [this] (->IndexedPersistentSet this nil nil nil))
  Object
  (-wrap [this] (-wrap (with-meta (vec this) (meta this)))))

(defprotocol Unwrap
  (-unwrap [coll]))

(extend-protocol Unwrap
  nil
  (-unwrap [coll] coll)
  Object
  (-unwrap [coll] coll)
  IndexedPersistentMap
  (-unwrap [coll] (with-meta (.-m ^IndexedPersistentMap coll) nil))
  IndexedPersistentVector
  (-unwrap [coll] (with-meta (.-v ^IndexedPersistentVector coll) nil))
  IndexedPersistentSet
  (-unwrap [coll] (with-meta (.-s ^IndexedPersistentSet coll) nil)))

(defn idx
  "Takes a set, vector or map and wraps it so that its elements support indexed queries.

  Indexes are created on demand to satisfy queries and then are reused.

  Indexes once realised will be maintained incrementally as you call conj, assoc and so on on the collection.

  The coll must be an associative collection (vector, map or set). If your collection is not associative, it is converted to a vector.

  Metadata is carried over to the new structure.

  If the collection is already indexed, it is returned as-is without modification."
  [coll]
  (-wrap coll))

(defn unwrap
  "Returns the backing collection without indexes."
  [coll]
  (-unwrap coll))

(defrecord Truthiness [p]
  Property
  (-property [this element] (boolean (-property p element))))


(defn group
  "Returns an (unordered) vector of items where (p element) equals v.

  p is a function, but it is expected that you use functions with equality semantics.

  The 2-ary form finds all elements where (p element) returns truthy."
  ([idx p] (group idx (->Truthiness p) true))
  ([idx p v]
   (let [i (-get-eq idx p)
         m (get i v)
         a (object-array (count m))]
     (reduce-kv (fn [i id _] (aset a (int i) (idx id)) (unchecked-inc-int i)) (int 0) m)
     (LazilyPersistentVector/create a))))

(defn identify
  "Returns the unique element where (-property p element) equals v.

  p is a function, but it is expected that you use functions with equality semantics."
  [idx p v]
  (let [i (-get-uniq idx p)
        id (get i v)]
    (when (some? id)
      (idx id))))

(defn ascending
  "Returns an ascending order seq of elements where (test (-property p element) v) returns true.

  The order is defined by the value of v.

  This is much like subseq in the clojure.core."
  [idx p test v]
  (let [i (-get-sorted idx p)]
    (if (some? i)
      (let [alist (ArrayList. 16)
            add-map (fn [_ id _] (.add alist (idx id)))]
        (->> (subseq i test v)
             (reduce (fn [e] (reduce-kv add-map nil (val e)))))
        (LazilyPersistentVector/createOwning (.toArray alist)))
      ())))

(defn descending
  "Returns a descending order seq of elements where (test (-property p element) v) returns true.

  The order is defined by the value of v.

  This is much like rsubseq in the clojure.core."
  [idx p test v]
  (let [i (-get-sorted idx p)]
    (if (some? i)
      (let [alist (ArrayList. 16)
            add-map (fn [_ id _] (.add alist (idx id)))]
        (->> (rsubseq i test v)
             (reduce (fn [e] (reduce-kv add-map nil (val e)))))
        (LazilyPersistentVector/createOwning (.toArray alist)))
      ())))

(extend-protocol Property
  Fn
  (-property [this element] (this element))
  Var
  (-property [this element] (this element))
  Keyword
  (-property [this element] (this element))
  Object
  (-property [this element] (get element this))
  nil
  (-property [this element] (get element nil)))

(deftype Prop [form f]
  Property
  (-property [this element] (f element))
  Object
  (equals [this obj] (and (instance? Prop obj) (= form (.-form ^Prop obj))))
  (hashCode [this] (.hashCode form)))

(defn- ->sym
  [x]
  (if (var? x)
    (let [^Var v x]
      (symbol (str (.name (.ns v))) (str (.sym v))))
    x))

(defn- unfn [expr]
  (if (and (seq? expr)
           (symbol? (first expr))
           (= "fn*" (name (first expr))))
    (let [[[s] & form] (rest expr)]
      (conj (walk/postwalk-replace {s '%} form) '[%] 'fn))
    expr))

(defn- res [form]
  (cond
    (keyword? form) form
    (symbol? form) (or (-> form resolve ->sym) form)
    (sequential? form) (walk/postwalk #(if (symbol? %) (res %) %) (unfn form))
    :else form))

(defmacro prop
  "Returns a property for the function form, gives it equality based on the form.

  The function MUST be pure, otherwise indexes WILL misbehave over time.

  Equality is defined as form equality."
  [form]
  (cond
    (keyword? form) form
    (var? form) form
    :else `(->Prop '~(res form) ~form)))

(defrecord AsKey [k]
  Property
  (-property [this element] (get element k)))

(defn as-key
  "Returns a property that looks up k as a key. Only useful if you are using functions or vars as keys."
  [k]
  (->AsKey k))

(defrecord Path [ks]
  Property
  (-property [this element] (get-in element ks)))

(defn path
  "Returns a property that will (get-in element k) for its value."
  [ks]
  (->Path ks))

(defrecord Selection [ks]
  Property
  (-property [this element] (select-keys element ks)))

(defn select
  "Returns a property that will (select-keys element k) for its value."
  [ks]
  (->Selection (set ks)))