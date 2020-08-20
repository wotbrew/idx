(ns com.wotbrew.impl.map
  (:require [com.wotbrew.impl.protocols :as p]
            [com.wotbrew.impl.index :as i])
  (:import (clojure.lang IKVReduce ArityException IPersistentCollection ILookup IPersistentMap Associative IMeta IObj MapEquivalence IHashEq IFn)
           (java.util Map$Entry Map)))

(deftype IndexedPersistentMap
  [m
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted
   ^boolean auto]
  p/Idx
  (-rewrap [idx a] (IndexedPersistentMap. m eq uniq sorted a))
  (-get-index [idx p kind]
    (case kind
      :idx/hash
      (or (get eq p)
          (when auto
            (let [i (i/create-eq-from-associative m p)
                  eq (assoc eq p i)]
              (set! (.-eq idx) eq)
              i)))

      :idx/unique
      (or (get uniq p)
          (when auto
            (let [i (i/create-uniq-from-associative m p)
                  uniq (assoc uniq p i)]
              (set! (.-uniq idx) uniq)
              i)))

      :idx/sort
      (or (get sorted p)
          (when auto
            (let [i (i/create-sorted-from-associative m p)
                  sorted (assoc sorted p i)]
              (set! (.-sorted idx) sorted)
              i)))))
  (-add-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        idx
        (IndexedPersistentMap. m (assoc eq p (i/create-eq-from-associative m p)) uniq sorted auto))
      :idx/unique
      (if (get uniq p)
        idx
        (IndexedPersistentMap. m eq (assoc uniq p (i/create-uniq-from-associative m p)) sorted auto))
      :idx/sort
      (if (get sorted p)
        idx
        (IndexedPersistentMap. m eq uniq (assoc sorted p (i/create-sorted-from-associative m p)) auto))))
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
  p/Wrap
  (-wrap [this a]
    (if (= a auto) this (p/-rewrap this a)))
  p/Unwrap
  (-unwrap [this] m)
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
          (some-> eq (i/add-eq o o1))
          (some-> uniq (i/add-uniq o o1))
          (some-> sorted (i/add-sorted o o1))
          auto)

        :else
        (IndexedPersistentMap.
          (.assoc ^IPersistentMap m o o1)
          (some-> eq (i/add-eq o old-element o1))
          (some-> uniq (i/add-uniq o old-element o1))
          (some-> sorted (i/add-sorted o old-element o1))
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
          (some-> eq (i/del-eq o old-element))
          (some-> uniq (i/del-uniq old-element))
          (some-> sorted (i/del-sorted o old-element))
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