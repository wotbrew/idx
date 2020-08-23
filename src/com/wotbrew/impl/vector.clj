(ns com.wotbrew.impl.vector
  (:require [com.wotbrew.impl.protocols :as p]
            [com.wotbrew.impl.index :as i])
  (:import (java.util RandomAccess List)
           (clojure.lang IHashEq Counted IObj IMeta IPersistentVector Seqable Reversible Indexed IPersistentCollection IPersistentStack ILookup Associative IFn Sequential ArityException IReduceInit IKVReduce)))

(deftype IndexedPersistentVector
  [v
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted
   ^boolean auto]
  p/Idx
  (-rewrap [idx a] (IndexedPersistentVector. v eq uniq sorted a))
  (-get-eq [idx p]
    (or (eq p)
        (when auto
          (let [i (i/create-eq-from-associative v p)
                eq (assoc eq p i)]
            (set! (.-eq idx) eq)
            i))))
  (-get-uniq [idx p]
    (or (uniq p)
        (when auto
          (let [i (i/create-uniq-from-associative v p)
                uniq (assoc uniq p i)]
            (set! (.-uniq idx) uniq)
            i))))
  (-get-sort [idx p]
    (or (sorted p)
        (when auto
          (let [i (i/create-sorted-from-associative v p)
                sorted (assoc sorted p i)]
            (set! (.-sorted idx) sorted)
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

        (identical? old-element ::not-found)
        (IndexedPersistentVector.
          (.assocN ^IPersistentVector v i val)
          (some-> eq (i/add-eq i val))
          (some-> uniq (i/add-uniq i val))
          (some-> sorted (i/add-sorted i val))
          auto)

        :else
        (IndexedPersistentVector.
          (.assocN ^IPersistentVector v i val)
          (some-> eq (i/add-eq i old-element val))
          (some-> uniq (i/add-uniq i old-element val))
          (some-> sorted (i/add-sorted i old-element val))
          auto))))
  (cons [this val]
    (let [i (.length ^IPersistentVector v)]
      (IndexedPersistentVector.
        (.cons ^IPersistentVector v val)
        (some-> eq (i/add-eq i val))
        (some-> uniq (i/add-uniq i val))
        (some-> sorted (i/add-sorted i val))
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
  (empty [this] (IndexedPersistentVector. (.empty ^IPersistentCollection v) {} {} {} auto))
  (equiv [this o] (.equiv ^IPersistentCollection v o))
  IPersistentStack
  (peek [this] (.peek ^IPersistentStack v))
  (pop [this]
    (let [i (dec (.length ^IPersistentVector v))
          old-element (if (neg? i) nil (nth v i))]
      (IndexedPersistentVector.
        (pop ^IPersistentStack v)
        (some-> eq (i/del-eq i old-element))
        (some-> uniq (i/del-uniq old-element))
        (some-> sorted (i/del-sorted i old-element))
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
  (kvreduce [this f init] (reduce-kv f init v))
  Comparable
  (compareTo [x y] (.compareTo ^Comparable v y)))