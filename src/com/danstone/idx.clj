(ns com.danstone.idx
  (:import (clojure.lang IPersistentMap Associative ILookup IPersistentCollection Seqable Counted MapEquivalence IHashEq IFn IMeta IObj ArityException IPersistentVector IPersistentSet IPersistentStack Indexed Reversible Sequential)
           (java.util Map$Entry Map List Set Collection RandomAccess)))

(set! *warn-on-reflection* true)

(defprotocol Idx
  "You should consider this protocol an implementation detail for now."
  (-element [idx id])
  (-get-uniq [idx c])
  (-get-eq [idx c])
  (-get-sorted [idx c]))

(defn- eq-add-element [eq id old-element element]
  (reduce-kv
    (fn [eq c i]
      (let [ov (c old-element)
            v (c element)]
        (if (identical? ov v)
          eq
          (let [oset (get i ov #{})
                oset (disj oset id)
                i (if (empty? oset) (dissoc i ov) (assoc i ov oset))

                nset (get i v #{})
                nset (conj nset id)
                i (assoc i v nset)]
            (assoc eq c i)))))
    eq
    eq))

(defn- eq-del-element [eq id old-element]
  (reduce-kv
    (fn [eq c i]
      (let [ov (c old-element)]
        (let [oset (get i ov #{})
              oset (disj oset id)
              i (if (empty? oset) (dissoc i ov) (assoc i ov oset))]
          (if (empty? i)
            (dissoc eq c)
            (assoc eq c i)))))
    eq
    eq))

(defn- uniq-add-element [unq id old-element element]
  (reduce-kv
    (fn [unq c i]
      (let [ov (c old-element)
            v (c element)]
        (if (identical? ov v)
          unq
          (let [i (dissoc i ov)
                i (assoc i v id)]
            (assoc unq c i)))))
    unq
    unq))

(defn- uniq-del-element [unq old-element]
  (reduce-kv
    (fn [unq c i]
      (let [ov (c old-element)]
        (let [i (dissoc i ov)]
          (if (empty? i)
            (dissoc unq c)
            (assoc unq c i)))))
    unq
    unq))

(defn- sorted-add-element [srt id old-element element]
  (reduce-kv
    (fn [srt c i]
      (let [ov (c old-element)
            v (c element)]
        (if (identical? ov v)
          srt
          (let [oset (get i ov #{})
                oset (disj oset id)
                i (if (empty? oset) (dissoc i ov) (assoc i ov oset))

                nset (get i v #{})
                nset (conj nset id)
                i (assoc i v nset)]
            (assoc srt c i)))))
    srt
    srt))

(defn- sorted-del-element [srt id old-element]
  (reduce-kv
    (fn [srt c i]
      (let [ov (c old-element)]
        (let [oset (get i ov #{})
              oset (disj oset id)
              i (if (empty? oset) (dissoc i ov) (assoc i ov oset))]
          (if (empty? i)
            (dissoc srt c)
            (assoc srt c i)))))
    srt
    srt))

(deftype IndexedPersistentMap
  [m
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted]
  Idx
  (-element [idx id] (get m id))
  (-get-eq [idx c]
    (or (get eq c)
        (let [rf (fn [m id v]
                   (let [ival (c v)]
                     (assoc! m ival (conj (get m ival #{}) id))))
              i (persistent! (reduce-kv rf (transient {}) m))
              eq (assoc eq c i)]
          (set! (.-eq idx) eq)
          i)))
  (-get-uniq [idx c]
    (or (get uniq c)
        (let [rf (fn [m id v]
                   (let [ival (c v)]
                     (assert (not (contains? m ival)))
                     (assoc! m ival id)))
              i (persistent! (reduce-kv rf (transient {}) m))
              uniq (assoc uniq c i)]
          (set! (.-uniq idx) uniq)
          i)))
  (-get-sorted [idx c]
    (or (get sorted c)
        (let [rf (fn [m id v]
                   (let [ival (c v)]
                     (assoc m ival (conj (get m ival #{}) id))))
              i (reduce-kv rf (sorted-map) m)
              srt (assoc sorted c i)]
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
  (invoke [this o o1] (.invoke ^IFn m o1))
  (invoke [this o o1 o2] (.invoke ^IFn m o1 o2))
  (applyTo [this arglist] (apply m arglist))

  IPersistentMap
  (assoc [this o o1]
    (let [old-element (get this o)]
      (if (identical? o1 old-element)
        (IndexedPersistentMap.
          (.assoc ^IPersistentMap m o o1)
          (some-> eq (eq-add-element o old-element o1))
          (some-> uniq (uniq-add-element o old-element o1))
          (some-> sorted (sorted-add-element o old-element o1))))))
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
          (some-> eq (eq-del-element o old-element))
          (some-> uniq (uniq-del-element old-element))
          (some-> sorted (sorted-del-element o old-element))))))
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
      (map? o)
      (reduce-kv assoc this o)
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
  (-element [idx id] (nth v id))
  (-get-eq [idx c]
    (or (get eq c)
        (let [rf (fn [m id v]
                   (let [ival (c v)]
                     (assoc! m ival (conj (get m ival #{}) id))))
              i (persistent! (reduce-kv rf (transient {}) v))
              eq (assoc eq c i)]
          (set! (.-eq idx) eq)
          i)))
  (-get-uniq [idx c]
    (or (get uniq c)
        (let [rf (fn [m id v]
                   (let [ival (c v)]
                     (assert (not (contains? m ival)))
                     (assoc! m ival id)))
              i (persistent! (reduce-kv rf (transient {}) v))
              uniq (assoc uniq c i)]
          (set! (.-uniq idx) uniq)
          i)))
  (-get-sorted [idx c]
    (or (get sorted c)
        (let [rf (fn [m id v]
                   (let [ival (c v)]
                     (assoc m ival (conj (get m ival #{}) id))))
              i (reduce-kv rf (sorted-map) v)
              srt (assoc sorted c i)]
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
    (let [old-element (nth this i)]
      (if (identical? val old-element)
        this
        (IndexedPersistentVector.
          v
          (some-> eq (eq-add-element i old-element val))
          (some-> uniq (uniq-add-element i old-element val))
          (some-> sorted (sorted-add-element i old-element val))))))
  (cons [this o]
    (let [i (.length ^IPersistentVector v)]
      (IndexedPersistentVector.
        (.cons ^IPersistentVector v val)
        (some-> eq (eq-add-element i nil val))
        (some-> uniq (uniq-add-element i nil val))
        (some-> sorted (sorted-add-element i nil val)))))
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
          old-element (if (neg? i) nil (nth this i))]
      (IndexedPersistentVector.
        (pop ^IPersistentStack v)
        (some-> eq (eq-del-element i old-element))
        (some-> uniq (uniq-del-element old-element))
        (some-> sorted (sorted-del-element i old-element)))))
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
  (-element [idx id] (get s id))
  (-get-eq [idx c]
    (or (get eq c)
        (let [rf (fn [m id]
                   (let [ival (c id)]
                     (assoc! m ival (conj (get m ival #{}) id))))
              i (persistent! (reduce rf (transient {}) s))
              eq (assoc eq c i)]
          (set! (.-eq idx) eq)
          i)))
  (-get-uniq [idx c]
    (or (get uniq c)
        (let [rf (fn [m id]
                   (let [ival (c id)]
                     (assert (not (contains? m ival)))
                     (assoc! m ival id)))
              i (persistent! (reduce rf (transient {}) s))
              uniq (assoc uniq c i)]
          (set! (.-uniq idx) uniq)
          i)))
  (-get-sorted [idx c]
    (or (get sorted c)
        (let [rf (fn [m id]
                   (let [ival (c id)]
                     (assoc m ival (conj (get m ival #{}) id))))
              i (reduce rf (sorted-map) s)
              srt (assoc sorted c i)]
          (set! (.-sorted idx) srt)
          i)))
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
  (containsAll [this c] (.containsAll ^Set s c))
  (addAll [this c] (throw (UnsupportedOperationException.)))
  (removeAll [this c] (throw (UnsupportedOperationException.)))
  (retainAll [this c] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  IPersistentSet
  (disjoin [this key]
    (let [ns (.disjoin ^IPersistentSet s key)]
      (if (identical? ns s)
        this
        (IndexedPersistentSet.
          ns
          (some-> eq (eq-del-element key key))
          (some-> uniq (uniq-del-element key))
          (some-> sorted (sorted-del-element key key))))))
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
          (some-> eq (eq-add-element o nil o))
          (some-> uniq (uniq-add-element o nil o))
          (some-> sorted (sorted-add-element o nil o))))))
  (empty [this] (IndexedPersistentSet. (.empty ^IPersistentSet s) nil nil nil))
  (equiv [this o] (.equiv ^IPersistentSet s o))
  Object
  (hashCode [this] (.hashCode s))
  (equals [this obj] (.equals s obj))
  (toString [this] (.toString s)))

(defprotocol Wrappable
  (-wrap [coll]))

(extend-protocol Wrappable
  IndexedPersistentMap (-wrap [this] this)
  IndexedPersistentVector (-wrap [this] this)
  IndexedPersistentSet (-wrap [this] this)
  IPersistentMap
  (-wrap [this] (->IndexedPersistentMap (with-meta this nil) nil nil nil))
  IPersistentVector
  (-wrap [this] (->IndexedPersistentVector (with-meta this nil) nil nil nil))
  IPersistentSet
  (-wrap [this] (->IndexedPersistentSet (with-meta this nil) nil nil nil))
  Object
  (-wrap [this] (-wrap (vec this))))

(defn idx
  "Takes a collection and wraps it so that its elements support indexed queries. Indexes are created on demand to satisfy queries
  and then are reused. Indexes once realised will be maintained incrementally.

  The coll must be an associative collection (vector, map or set). If your collection is not associative, it is converted to a vector."
  [coll]
  (-wrap coll))

(defn lookup [idx c v]
  (let [i (-get-eq idx c)
        set (get i v)]
    (map (partial -element idx) set)))

(defn identify [idx c v]
  (let [i (-get-uniq idx c)
        id (get i v)]
    (when (some? id)
      (-element idx id))))

(defn ascending [idx c test v]
  (let [i (-get-sorted idx c)]
    (if (some? i)
      (->> (subseq i test v)
           (mapcat val)
           (map (partial -element idx)))
      ())))

(defn descending [idx c test v]
  (let [i (-get-sorted idx c)]
    (if (some? i)
      (->> (rsubseq i test v)
           (mapcat val)
           (map (partial -element idx)))
      ())))

(deftype Path [ks] IFn (invoke [this o] (get-in o ks)))
(defn path [ks] (->Path ks))