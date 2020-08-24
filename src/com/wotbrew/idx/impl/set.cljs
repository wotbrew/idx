(ns com.wotbrew.idx.impl.set
  (:require [com.wotbrew.idx.impl.protocols :as p]
            [com.wotbrew.idx.impl.index :as i]))

(deftype IndexedPersistentSet
  [s
   ^:mutable eq
   ^:mutable uniq
   ^:mutable sorted
   auto]
  p/Idx
  (-rewrap [idx a] (IndexedPersistentSet. s eq uniq sorted a))
  (-get-eq [idx p]
    (or (when (some? eq) (eq p))
        (when auto
          (let [i (i/create-eq-from-elements s p)
                neq (assoc eq p i)]
            (set! eq neq)
            i))))
  (-get-uniq [idx p]
    (or (when (some? uniq) (uniq p))
        (when auto
          (let [i (i/create-unique-from-elements s p)
                nuniq (assoc uniq p i)]
            (set! uniq nuniq)
            i))))
  (-get-sort [idx p]
    (or (when (some? sorted) (sorted p))
        (when auto
          (let [i (i/create-sorted-from-elements s p)
                nsorted (assoc sorted p i)]
            (set! sorted nsorted)
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
  Object
  (toString [coll]
    (str s))
  (equiv [this other]
    (-equiv this other))

  ;; EXPERIMENTAL: subject to change
  (keys [coll]
    (es6-iterator (seq coll)))
  (entries [coll]
    (es6-set-entries-iterator (seq coll)))
  (values [coll]
    (es6-iterator (seq coll)))
  (has [coll k]
    (contains? coll k))
  (forEach [coll f]
    (doseq [[k v] coll]
      (f v k)))

  IPrintWithWriter
  (-pr-writer [coll writer opts] (-pr-writer s writer opts))

  ICloneable
  (-clone [_] (IndexedPersistentSet. s eq uniq sorted auto))

  IIterable
  (-iterator [coll] (-iterator s))

  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (IndexedPersistentSet. (with-meta s new-meta) eq uniq sorted auto)))

  IMeta
  (-meta [coll] (meta s))

  ICollection
  (-conj [coll o]
    (let [ns (-conj s o)]
      (if (identical? ns s)
        coll
        (IndexedPersistentSet.
          ns
          (some-> eq (i/add-eq o o))
          (some-> uniq (i/add-uniq o o))
          (some-> sorted (i/add-sorted o o))
          auto))))

  IEmptyableCollection
  (-empty [coll] (IndexedPersistentSet. (-empty s) nil nil nil auto))

  IEquiv
  (-equiv [coll other] (-equiv s other))

  IHash
  (-hash [coll] (-hash s))

  ISeqable
  (-seq [coll] (-seq s))

  ICounted
  (-count [coll] (-count s))

  ILookup
  (-lookup [coll v] (-lookup s v nil))
  (-lookup [coll v not-found] (-lookup s v not-found))

  ISet
  (-disjoin [coll v]
    (let [ns (-disjoin s v)]
      (if (identical? ns s)
        coll
        (IndexedPersistentSet.
          ns
          (some-> eq (i/del-eq v v))
          (some-> uniq (i/del-uniq v))
          (some-> sorted (i/del-sorted v v))
          auto))))

  IFn
  (-invoke [coll k]
    (-lookup s k))
  (-invoke [coll k not-found]
    (-lookup s k not-found)))

(es6-iterable IndexedPersistentSet)