(ns com.wotbrew.impl.vector
  (:require [com.wotbrew.impl.index :as i]
            [com.wotbrew.impl.protocols :as p]))

(deftype IndexedPersistentVector
  [v
   ^:mutable eq
   ^:mutable uniq
   ^:mutable sorted
   auto]
  p/Idx
  (-rewrap [idx a] (IndexedPersistentVector. v eq uniq sorted a))
  (-get-eq [idx p]
    (or (when (some? eq) (eq p))
        (when auto
          (let [i (i/create-eq-from-associative v p)
                neq (assoc eq p i)]
            (set! eq neq)
            i))))
  (-get-uniq [idx p]
    (or (when (some? uniq) (uniq p))
        (when auto
          (let [i (i/create-uniq-from-associative v p)
                nuniq (assoc uniq p i)]
            (set! uniq nuniq)
            i))))
  (-get-sort [idx p]
    (or (when (some? sorted) (sorted p))
        (when auto
          (let [i (i/create-sorted-from-associative v p)
                nsorted (assoc sorted p i)]
            (set! sorted nsorted)
            i))))
  (-add-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        idx
        (IndexedPersistentVector. v (assoc eq p (i/create-eq-from-associative v p)) uniq sorted auto))
      :idx/unique
      (if (get uniq p)
        idx
        (IndexedPersistentVector. v eq (assoc uniq p (i/create-uniq-from-associative v p)) sorted auto))
      :idx/sort
      (if (get sorted p)
        idx
        (IndexedPersistentVector. v eq uniq (assoc sorted p (i/create-sorted-from-associative v p)) auto))))
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
  p/Wrap
  (-wrap [this a]
    (if (= a auto) this (p/-rewrap this a)))
  p/Unwrap
  (-unwrap [this] v)
  Object
  (toString [coll] (str v))
  (equiv [this other]
    (-equiv this other))
  (indexOf [coll x]
    (-indexOf coll x 0))
  (indexOf [coll x start]
    (-indexOf coll x start))
  (lastIndexOf [coll x]
    (-lastIndexOf coll x (count coll)))
  (lastIndexOf [coll x start]
    (-lastIndexOf coll x start))
  IPrintWithWriter
  (-pr-writer [coll writer opts] (-pr-writer v writer opts))
  ICloneable
  (-clone [_] (IndexedPersistentVector. v eq uniq sorted auto))
  IMeta
  (-meta [coll] (meta v))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta (meta v))
      coll
      (IndexedPersistentVector. (with-meta v new-meta) eq uniq sorted auto)))
  IStack
  (-peek [coll]
    (-peek v))
  (-pop [coll]
    (let [i (dec (-count v))
          old-element (if (neg? i) nil (-nth v i))]
      (IndexedPersistentVector.
        (-pop v)
        (some-> eq (i/del-eq i old-element))
        (some-> uniq (i/del-uniq old-element))
        (some-> sorted (i/del-sorted i old-element))
        auto)))
  ICollection
  (-conj [coll o]
    (let [i (-count v)]
      (IndexedPersistentVector.
        (-conj v o)
        (some-> eq (i/add-eq i o))
        (some-> uniq (i/add-uniq i o))
        (some-> sorted (i/add-sorted i o))
        auto)))
  IEmptyableCollection
  (-empty [coll]
    (IndexedPersistentVector. (-empty v) nil nil nil auto))
  ISequential
  IEquiv
  (-equiv [coll other] (-equiv v other))
  IHash
  (-hash [coll] (-hash v))
  ISeqable
  (-seq [coll] (-seq v))
  ICounted
  (-count [coll] (-count v))
  IIndexed
  (-nth [coll n] (-nth v n))
  (-nth [coll n not-found] (-nth v n not-found))
  ILookup
  (-lookup [coll k] (-lookup v k))
  (-lookup [coll k not-found] (-lookup v k not-found))
  IAssociative
  (-assoc [coll k v]
    (if (number? k)
      (-assoc-n coll k v)
      (throw (js/Error. "Vector's key for assoc must be a number."))))
  (-contains-key? [coll k] (-contains-key? v k))
  IFind
  (-find [coll n] (-find v n))
  IVector
  (-assoc-n [coll i val]
    (let [old-element (-nth v i ::not-found)]
      (cond
        (identical? val old-element) coll
        (identical? old-element ::not-found)
        (IndexedPersistentVector.
          (-assoc-n v i val)
          (some-> eq (i/add-eq i val))
          (some-> uniq (i/add-uniq i val))
          (some-> sorted (i/add-sorted i val))
          auto)
        :else
        (IndexedPersistentVector.
          (-assoc-n v i val)
          (some-> eq (i/add-eq i old-element val))
          (some-> uniq (i/add-uniq i old-element val))
          (some-> sorted (i/add-sorted i old-element val))
          auto))))
  IReduce
  (-reduce [coll f] (-reduce v f))
  (-reduce [coll f init] (-reduce v f init))
  IKVReduce
  (-kv-reduce [coll f init] (-kv-reduce v f init))
  IFn
  (-invoke [coll k] (-lookup v k))
  (-invoke [coll k not-found] (-lookup v k not-found))
  IReversible
  (-rseq [coll] (-rseq v))
  IIterable
  (-iterator [this] (-iterator v))
  IComparable
  (-compare [x y] (-compare v y)))

(es6-iterable IndexedPersistentVector)