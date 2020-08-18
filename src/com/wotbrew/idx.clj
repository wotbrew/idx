(ns com.wotbrew.idx
  (:import (clojure.lang IPersistentMap Associative ILookup IPersistentCollection Seqable Counted MapEquivalence IHashEq IFn IMeta IObj ArityException IPersistentVector IPersistentSet IPersistentStack Indexed Reversible Sequential Keyword Var Fn LazilyPersistentVector IKVReduce IReduce IReduceInit PersistentArrayMap)
           (java.util Map$Entry Map List Set Collection RandomAccess ArrayList)))

(set! *warn-on-reflection* true)

(defprotocol Property
  "You should consider this protocol an implementation detail for now."
  (-property [this element]))

(defprotocol Predicate
  "You should consider this protocol an implementation detail for now."
  (-prop [this])
  (-val [this]))

(defprotocol Idx
  "You should consider this protocol an implementation detail for now."
  (-rewrap [idx auto])
  (-get-index [idx p kind])
  (-del-index [idx p kind])
  (-add-index [idx p kind])
  (-elements [idx])
  (-id-element-pairs [idx]))

(defprotocol Wrap
  "You should consider this protocol an implementation detail for now."
  (-wrap [coll auto]))

(defprotocol Unwrap
  "You should consider this protocol an implementation detail for now."
  (-unwrap [coll]))

(extend-protocol
  Idx
  nil
  (-rewrap [coll auto] nil)
  (-get-index [coll p kind] nil)
  (-del-index [coll p kind] nil)
  (-add-index [coll p kind] (-> (-wrap coll false) (-add-index p kind)))
  (-elements [coll] nil)
  (-id-element-pairs [coll] nil)
  Object
  (-rewrap [coll auto] coll)
  (-get-index [coll p kind] nil)
  (-del-index [coll p kind] coll)
  (-add-index [coll p kind] (-> (-wrap coll false) (-add-index p kind)))
  (-elements [coll] (-elements (-wrap coll false)))
  (-id-element-pairs [coll] (-id-element-pairs (-wrap coll false))))

(defn- create-eq-from-associative
  [m p]
  (let [rf (fn [m id v]
             (let [ival (-property p v)]
               (assoc! m ival (assoc (get m ival {}) id id))))]
    (persistent! (reduce-kv rf (transient {}) m))))

(defn- create-eq-from-elements
  [elements p]
  (let [rf (fn [m v]
             (let [ival (-property p v)]
               (assoc! m ival (assoc (get m ival {}) v v))))]
    (persistent! (reduce rf (transient {}) elements))))

(defn- create-uniq-from-associative
  [m p]
  (let [rf (fn [m id v]
             (let [ival (-property p v)]
               (assoc! m ival id)))]
    (persistent! (reduce-kv rf (transient {}) m))))

(defn- create-unique-from-elements
  [elements p]
  (let [rf (fn [m v]
             (let [ival (-property p v)]
               (assoc! m ival v)))]
    (persistent! (reduce rf (transient {}) elements))))

(defn- create-sorted-from-associative
  [m p]
  (let [rf (fn [m id v]
             (let [ival (-property p v)]
               (assoc m ival (assoc (get m ival {}) id id))))]
    (reduce-kv rf (sorted-map) m)))

(defn- create-sorted-from-elements
  [elements p]
  (let [rf (fn [m v]
             (let [ival (-property p v)]
               (assoc m ival (assoc (get m ival {}) v v))))]
    (reduce rf (sorted-map) elements)))

;; for eq/sorted leaves we use maps rather than sets so
;; we get the PersistentArrayMap optimisation when small.

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
         (cond
           (and (identical? id (i v)) (identical? ov v)) unq
           (identical? ov v) (assoc unq p (assoc i v id))
           :else
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
   ^:unsynchronized-mutable sorted
   ^boolean auto]
  Idx
  (-rewrap [idx auto] (IndexedPersistentMap. m eq uniq sorted auto))
  (-get-index [idx p kind]
    (case kind
      :idx/hash
      (or (get eq p)
          (when auto
            (let [i (create-eq-from-associative m p)
                  eq (assoc eq p i)]
              (set! (.-eq idx) eq)
              i)))

      :idx/unique
      (or (get uniq p)
          (when auto
            (let [i (create-uniq-from-associative m p)
                  uniq (assoc uniq p i)]
              (set! (.-uniq idx) uniq)
              i)))

      :idx/sort
      (or (get sorted p)
          (when auto
            (let [i (create-sorted-from-associative m p)
                  sorted (assoc eq p i)]
              (set! (.-sorted idx) sorted)
              i)))))
  (-add-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        idx
        (IndexedPersistentMap. m (assoc eq p (create-eq-from-associative m p)) uniq sorted auto))
      :idx/unique
      (if (get uniq p)
        idx
        (IndexedPersistentMap. m eq (assoc uniq p (create-uniq-from-associative m p)) sorted auto))
      :idx/sort
      (if (get sorted p)
        idx
        (IndexedPersistentMap. m eq uniq (assoc sorted p (create-sorted-from-associative m p)) auto))))
  (-del-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        (IndexedPersistentMap. m (dissoc eq p) uniq sorted auto)
        idx)
      :idx/unique
      (if (get uniq p)
        (IndexedPersistentMap. m eq (dissoc uniq p) sorted auto)
        idx)
      :idx/sort
      (if (get sorted p)
        (IndexedPersistentMap. m eq uniq (dissoc sorted p) auto)
        idx)))
  (-elements [idx] (vals m))
  (-id-element-pairs [idx] (map (juxt key val) m))
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
  (withMeta [this mta] (IndexedPersistentMap. (.withMeta ^IObj m mta) eq uniq sorted auto))

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
          (some-> sorted (add-sorted o o1))
          auto)

        :else
        (IndexedPersistentMap.
          (.assoc ^IPersistentMap m o o1)
          (some-> eq (add-eq o old-element o1))
          (some-> uniq (add-uniq o old-element o1))
          (some-> sorted (add-sorted o old-element o1))
          auto))))
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
          (some-> sorted (del-sorted o old-element))
          auto))))
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
  (empty [this] (IndexedPersistentMap. (.empty ^IPersistentCollection m) nil nil nil auto))
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
  (run [this] (throw (ArityException. 0 "IndexedPersistentMap")))
  IKVReduce
  (kvreduce [this f init] (reduce-kv f init m)))

(deftype IndexedPersistentVector
  [v
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted
   ^boolean auto]
  Idx
  (-rewrap [idx auto] (IndexedPersistentVector. v eq uniq sorted auto))
  (-get-index [idx p kind]
    (case kind
      :idx/hash
      (or (get eq p)
          (when auto
            (let [i (create-eq-from-associative v p)
                  eq (assoc eq p i)]
              (set! (.-eq idx) eq)
              i)))

      :idx/unique
      (or (get uniq p)
          (when auto
            (let [i (create-uniq-from-associative v p)
                  uniq (assoc uniq p i)]
              (set! (.-uniq idx) uniq)
              i)))

      :idx/sort
      (or (get sorted p)
          (when auto
            (let [i (create-sorted-from-associative v p)
                  sorted (assoc eq p i)]
              (set! (.-sorted idx) sorted)
              i)))))
  (-add-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        idx
        (IndexedPersistentVector. v (assoc eq p (create-eq-from-associative v p)) uniq sorted auto))
      :idx/unique
      (if (get uniq p)
        idx
        (IndexedPersistentVector. v eq (assoc uniq p (create-uniq-from-associative v p)) sorted auto))
      :idx/sort
      (if (get sorted p)
        idx
        (IndexedPersistentVector. v eq uniq (assoc sorted p (create-sorted-from-associative v p)) auto))))
  (-del-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        (IndexedPersistentVector. v (dissoc eq p) uniq sorted auto)
        idx)
      :idx/unique
      (if (get uniq p)
        (IndexedPersistentVector. v eq (dissoc uniq p) sorted auto)
        idx)
      :idx/sort
      (if (get sorted p)
        (IndexedPersistentVector. v eq uniq (dissoc sorted p) auto)
        idx)))
  (-elements [idx] v)
  (-id-element-pairs [idx] (map-indexed vector v))
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
  (withMeta [this meta] (IndexedPersistentVector. (.withMeta ^IObj v meta) eq uniq sorted auto))
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
          (some-> sorted (add-sorted i val))
          auto)

        :else
        (IndexedPersistentVector.
          (.assocN ^IPersistentVector v i val)
          (some-> eq (add-eq i old-element val))
          (some-> uniq (add-uniq i old-element val))
          (some-> sorted (add-sorted i old-element val))
          auto))))
  (cons [this val]
    (let [i (.length ^IPersistentVector v)]
      (IndexedPersistentVector.
        (.cons ^IPersistentVector v val)
        (some-> eq (add-eq i val))
        (some-> uniq (add-uniq i val))
        (some-> sorted (add-sorted i val))
        auto)))
  Seqable
  (seq [this] (.seq ^Seqable v))
  Reversible
  (rseq [this] (.rseq ^Reversible v))
  Indexed
  (nth [this i] (.nth ^Indexed v i))
  (nth [this i notFound] (.nth ^Indexed v i notFound))
  IPersistentCollection
  (count [this] (.count ^IPersistentCollection v))
  (empty [this] (IndexedPersistentVector. (.empty ^IPersistentCollection v) eq uniq sorted auto))
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
        (some-> sorted (del-sorted i old-element))
        auto)))
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
  RandomAccess
  IFn
  (invoke [this arg1] (.invoke ^IFn v arg1))
  (invoke [this arg1 arg2] (.invoke ^IFn v arg1 arg2))
  (applyTo [this arglist] (apply v arglist))
  Callable
  (call [this] (throw (ArityException. 0 "IndexedPersistentVector")))
  Runnable
  (run [this] (throw (ArityException. 0 "IndexedPersistentVector")))
  IReduceInit
  (reduce [this f init] (reduce f init v))
  IKVReduce
  (kvreduce [this f init] (reduce-kv f init v)))

(deftype IndexedPersistentSet
  [s
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted
   ^boolean auto]
  Idx
  (-rewrap [idx auto] (IndexedPersistentSet. s eq uniq sorted auto))
  (-get-index [idx p kind]
    (case kind
      :idx/hash
      (or (get eq p)
          (when auto
            (let [i (create-eq-from-elements s p)
                  eq (assoc eq p i)]
              (set! (.-eq idx) eq)
              i)))

      :idx/unique
      (or (get uniq p)
          (when auto
            (let [i (create-unique-from-elements s p)
                  uniq (assoc uniq p i)]
              (set! (.-uniq idx) uniq)
              i)))

      :idx/sort
      (or (get sorted p)
          (when auto
            (let [i (create-sorted-from-elements s p)
                  sorted (assoc eq p i)]
              (set! (.-sorted idx) sorted)
              i)))))
  (-add-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        idx
        (IndexedPersistentSet. s (assoc eq p (create-eq-from-elements s p)) uniq sorted auto))
      :idx/unique
      (if (get uniq p)
        idx
        (IndexedPersistentSet. s eq (assoc uniq p (create-unique-from-elements s p)) sorted auto))
      :idx/sort
      (if (get sorted p)
        idx
        (IndexedPersistentSet. s eq uniq (assoc sorted p (create-sorted-from-elements s p)) auto))))
  (-del-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        (IndexedPersistentSet. s (dissoc eq p) uniq sorted auto)
        idx)
      :idx/unique
      (if (get uniq p)
        (IndexedPersistentSet. s eq (dissoc uniq p) sorted auto)
        idx)
      :idx/sort
      (if (get sorted p)
        (IndexedPersistentSet. s eq uniq (dissoc sorted p) auto)
        idx)))
  (-elements [idx] s)
  (-id-element-pairs [idx] (map (fn [x] [x x]) s))
  IObj
  (withMeta [this meta] (IndexedPersistentSet. (.withMeta ^IObj s meta) eq uniq sorted auto))
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
          (some-> sorted (del-sorted key key))
          auto))))
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
          (some-> sorted (add-sorted o o))
          auto))))
  (empty [this] (IndexedPersistentSet. (.empty ^IPersistentSet s) nil nil nil auto))
  (equiv [this o] (.equiv ^IPersistentSet s o))
  Object
  (hashCode [this] (.hashCode s))
  (equals [this obj] (.equals s obj))
  (toString [this] (.toString s)))

(extend-protocol Wrap
  IndexedPersistentMap
  (-wrap [^IndexedPersistentMap this auto]
    (if (= auto (.-auto this))
      this
      (-rewrap this auto)))
  IndexedPersistentVector
  (-wrap [^IndexedPersistentVector this auto]
    (if (= auto (.-auto this))
      this
      (-rewrap this auto)))
  IndexedPersistentSet
  (-wrap [this auto]
    (if (= auto (.-auto this))
      this
      (-rewrap this auto)))
  IPersistentMap
  (-wrap [this auto] (->IndexedPersistentMap this nil nil nil auto))
  IPersistentVector
  (-wrap [this auto] (->IndexedPersistentVector this nil nil nil auto))
  IPersistentSet
  (-wrap [this auto] (->IndexedPersistentSet this nil nil nil auto))
  nil
  (-wrap [this auto] (-wrap [] auto))
  Object
  (-wrap [this auto] (-wrap (with-meta (vec this) (meta this)) auto)))

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

(defn auto-idx
  "Takes a set, vector or map and wraps it so that its elements support indexed queries.

  Indexes are created on demand to satisfy queries and then are reused.

  Indexes once realised will be maintained incrementally as you call conj, assoc and so on on the collection.

  The coll must be a vector, map or set. If you pass a seq/seqable/iterable it is converted to a vector.

  Metadata is carried over to the new structure.

  If the collection is already indexed, it is returned as-is without modification."
  [coll]
  (-wrap coll true))

(defn unwrap
  "Returns the backing collection without indexes."
  [coll]
  (-unwrap coll))

(defn idx
  "Adds indexes to the collection, returning a new indexed collection.

  Specify a property/predicate to index, and a `kind` being:

  `:idx/unique` (for identify and replace-by calls)
  `:idx/hash (for group calls)`
  `:idx/sorted` (for ascending/descending calls)"
  ([coll] coll)
  ([coll p kind] (-add-index coll p kind))
  ([coll p kind & more]
    (loop [coll (-add-index coll p kind)
           more more]
      (if-some [[p kind & more] (seq more)]
        (recur (-add-index coll p kind) more)
        coll))))

(defn delete-index
  "Deletes indexes from the collection, returning a new collection."
  ([coll p kind] (-del-index coll p kind))
  ([coll p kind & more]
   (loop [coll (-del-index coll p kind)
          more more]
     (if-some [[p kind & more] (seq more)]
       (recur (-del-index coll p kind) more)
       coll))))

(defrecord Comp [p1 p2]
  Property
  (-property [this element] (-property p1 (-property p2 element))))

(defn pcomp
  "Like clojure.core/comp but on properties."
  [p1 p2]
  (->Comp p1 p2))

(defrecord Select [ps]
  Property
  (-property [this element]
    (reduce (fn [m p] (assoc m p (-property p element))) {} ps)))

(defrecord Pred [p v]
  Predicate
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
    (build-match-map m (pcomp (-prop v) p) (-val v))
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

(defn group
  "Returns an (unordered) vector of items where (p element) equals v.

  The 2-ary takes a **predicate** which composes a property with its expected value, either a `(match)` form, or a `(pred)` form."
  ([coll pred] (group coll (-prop pred) (-val pred)))
  ([coll p v]
   (if (instance? Pred v)
     (group coll (pcomp (-prop v) p) (-val v))
     (if-some [i (-get-index coll p :idx/hash)]
       (let [m (get i v {})]
         (if (<= (count m) 32)
           (let [a (object-array (count m))]
             (reduce-kv (fn [i id _] (aset a (int i) (coll id)) (unchecked-inc-int i)) (int 0) m)
             (LazilyPersistentVector/createOwning a))
           (persistent! (transduce (map coll) conj! (transient []) (vals m)))))
       (filterv (fn [element] (= v (-property p element))) coll)))))

(defn identify
  "Returns the unique element where the property equals v."
  ([coll pred] (identify coll (-prop pred) (-val pred)))
  ([coll p v]
   (if (instance? Pred v)
     (identify coll (pcomp (-prop v) p) (-val v))
     (if-some [i (-get-index coll p :idx/unique)]
       (let [id (get i v)]
         (when (some? id)
           (coll id)))
       (some (fn [element] (when (= v (-property p element)) element)) (-elements coll))))))

(defn replace-by
  "Replaces an element by an alternative key."
  ([coll pred element] (replace-by coll (-prop pred) (-val pred) element))
  ([coll p v element]
   (if (instance? Pred v)
     (replace-by coll (pcomp (-prop v) p) (-val v) element)
     (if-some [i (-get-index coll p :idx/unique)]
       (let [id (get i v)]
         (if (associative? coll)
           (assoc coll id element)
           (-> coll (disj id) (conj element))))
       (let [id (some (fn [[id element]] (when (= v (-property p element)) id)) (-elements coll))]
         (if (associative? coll)
           (assoc coll id element)
           (-> coll (disj id) (conj element))))))))

(defn ascending
  "Returns an ascending order seq of elements where (test (p element) v) returns true.

  The order is defined by the value of v.

  This is much like subseq in the clojure.core."
  [coll p test v]
  (let [i (-get-index coll p :index/sorted)
        i (or i (create-sorted-from-elements (-elements coll) p))]
    (if (some? i)
      (let [alist (ArrayList. 16)
            add-map (fn [_ id _] (.add alist (coll id)))]
        (->> (subseq i test v)
             (reduce (fn [e] (reduce-kv add-map nil (val e)))))
        (if (<= (.size alist) 32)
          (LazilyPersistentVector/createOwning (.toArray alist))
          (into [] alist)))
      ())))

(defn descending
  "Returns a descending order seq of elements where (test (p element) v) returns true.

  The order is defined by the value of v.

  This is much like rsubseq in the clojure.core."
  [coll p test v]
  (let [i (-get-index coll p :index/sorted)
        i (or i (create-sorted-from-elements (-elements coll) p))]
    (if (some? i)
      (let [alist (ArrayList. 16)
            add-map (fn [_ id _] (.add alist (coll id)))]
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

(defrecord AsKey [k]
  Property
  (-property [this element] (get element k)))

(defn as-key
  "Returns a property that looks up k as a key. Only useful if you are using functions as keys."
  [k]
  (->AsKey k))

(defrecord Path2 [a b]
  Property
  (-property [this element] (->> element (-property a) (-property b))))

(defrecord Path3 [a b c]
  Property
  (-property [this element] (->> element (-property a) (-property b) (-property c))))

(defrecord Path [ps]
  Property
  (-property [this element]
    (reduce
      (fn [v p] (-property p v))
      element
      ps)))

(defn path
  "Returns a property that will drill down to some nested value by using the properties in the list. Think get-in."
  ([p] p)
  ([p1 p2] (->Path2 p1 p2))
  ([p1 p2 p3] (->Path3 p1 p2 p3))
  ([p1 p2 p3 p4 & more] (->Path (reduce conj [p1 p2 p3 p4] more))))