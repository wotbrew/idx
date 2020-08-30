(ns com.wotbrew.idx.impl.set
  (:require [com.wotbrew.idx.impl.protocols :as p]
            [com.wotbrew.idx.impl.index :as i])
  (:import (clojure.lang IPersistentSet IPersistentCollection Seqable IFn IHashEq IMeta IObj)
           (java.util Set Collection)))

(deftype IndexedPersistentSet
  [s
   ^:unsynchronized-mutable eq
   ^:unsynchronized-mutable uniq
   ^:unsynchronized-mutable sorted
   ^boolean auto]
  p/Idx
  (-rewrap [idx a] (IndexedPersistentSet. s eq uniq sorted a))
  (-get-eq [idx p]
    (or (when (some? eq) (eq p))
        (when auto
          (let [i (i/create-eq-from-elements s p)
                eq (assoc eq p i)]
            (set! (.-eq idx) eq)
            i))))
  (-get-uniq [idx p]
    (or (when (some? uniq) (uniq p))
        (when auto
          (let [i (i/create-unique-from-elements s p)
                uniq (assoc uniq p i)]
            (set! (.-uniq idx) uniq)
            i))))
  (-get-sort [idx p]
    (or (when (some? sorted) (sorted p))
        (when auto
          (let [i (i/create-sorted-from-elements s p)
                sorted (assoc sorted p i)]
            (set! (.-sorted idx) sorted)
            i))))
  (-add-index [idx p kind]
    (case kind
      :idx/hash
      (if (get eq p)
        idx
        (IndexedPersistentSet. s (assoc eq p (i/create-eq-from-elements s p)) uniq sorted auto))
      :idx/unique
      (if (get uniq p)
        idx
        (IndexedPersistentSet. s eq (assoc uniq p (i/create-unique-from-elements s p)) sorted auto))
      :idx/sort
      (if (get sorted p)
        idx
        (IndexedPersistentSet. s eq uniq (assoc sorted p (i/create-sorted-from-elements s p)) auto))))
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
  p/Wrap
  (-wrap [this a]
    (if (= a auto) this (p/-rewrap this a)))
  p/Unwrap
  (-unwrap [this] s)
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
  (^objects toArray [this ^objects a] (.toArray ^Set s a))
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
          (some-> eq (i/del-eq key key))
          (some-> uniq (i/del-uniq key))
          (some-> sorted (i/del-sorted key key))
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
          (some-> eq (i/add-eq o o))
          (some-> uniq (i/add-uniq o o))
          (some-> sorted (i/add-sorted o o))
          auto))))
  (empty [this] (IndexedPersistentSet. (.empty ^IPersistentSet s) nil nil nil auto))
  (equiv [this o] (.equiv ^IPersistentSet s o))
  Object
  (hashCode [this] (.hashCode s))
  (equals [this obj] (.equals s obj))
  (toString [this] (.toString s)))