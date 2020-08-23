(ns com.wotbrew.impl.map
  (:require [com.wotbrew.impl.index :as i]
            [com.wotbrew.impl.protocols :as p]))

(deftype IndexedPersistentMap
  [m
   ^:mutable eq
   ^:mutable uniq
   ^:mutable sorted
   auto]

  p/Idx
  (-rewrap [idx a] (IndexedPersistentMap. m eq uniq sorted a))
  (-get-eq [idx p]
    (or (eq p)
        (when auto
          (let [i (i/create-eq-from-associative m p)
                neq (assoc eq p i)]
            (set! eq neq)
            i))))
  (-get-uniq [idx p]
    (or (uniq p)
        (when auto
          (let [i (i/create-uniq-from-associative m p)
                nuniq (assoc uniq p i)]
            (set! uniq nuniq)
            i))))
  (-get-sort [idx p]
    (or (sorted p)
        (when auto
          (let [i (i/create-sorted-from-associative m p)
                nsorted (assoc sorted p i)]
            (set! sorted nsorted)
            i))))
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
  Object
  (toString [coll]
    (str m))
  (equiv [this other]
    (-equiv m other))

  (keys [coll]
    (es6-iterator (keys coll)))
  (entries [coll]
    (es6-entries-iterator (seq coll)))
  (values [coll]
    (es6-iterator (vals coll)))
  (has [coll k]
    (contains? coll k))
  (get [coll k not-found]
    (-lookup coll k not-found))
  (forEach [coll f]
    (doseq [[k v] coll]
      (f v k)))
  IPrintWithWriter
  (-pr-writer [coll writer opts] (-pr-writer m writer opts))
  ICloneable
  (-clone [_] (IndexedPersistentMap. m eq uniq sorted auto))
  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta (meta m))
      coll
      (IndexedPersistentMap. (with-meta m new-meta) eq uniq sorted auto)))
  IMeta
  (-meta [coll] (meta m))
  ICounted
  (-count [coll] (-count m))
  IAssociative
  (-contains-key? [coll k] (-contains-key? m k))
  (-assoc [coll k element]
    (let [old-element (-lookup m k ::not-found)]
      (cond
        (identical? element old-element) coll
        (identical? old-element ::not-found)
        (IndexedPersistentMap.
          (-assoc m k element)
          (some-> eq (i/add-eq k element))
          (some-> uniq (i/add-uniq k element))
          (some-> sorted (i/add-sorted k element))
          auto)
        :else
        (IndexedPersistentMap.
          (-assoc m k element)
          (some-> eq (i/add-eq k old-element element))
          (some-> uniq (i/add-uniq k old-element element))
          (some-> sorted (i/add-sorted k old-element element))
          auto))))
  IMap
  (-dissoc [coll k]
    (let [old-element (-lookup m k ::not-found)]
      (if (identical? ::not-found old-element)
        coll
        (IndexedPersistentMap.
          (-dissoc m k)
          (some-> eq (i/del-eq k old-element))
          (some-> uniq (i/del-uniq old-element))
          (some-> sorted (i/del-sorted k old-element))
          auto))))
  IFind
  (-find [coll k] (-find m k))
  ISeqable
  (-seq [o] (-seq m))
  IFn
  (-invoke [coll k] (-lookup m k))
  (-invoke [coll k not-found] (-lookup m k not-found))
  ILookup
  (-lookup [coll k] (-lookup m k))
  (-lookup [coll k not-found] (-lookup m k not-found))
  IEmptyableCollection
  (-empty [coll] (IndexedPersistentMap. {} {} {} {} auto))
  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (loop [ret coll
             es (seq entry)]
        (if (nil? es)
          ret
          (let [e (first es)]
            (if (vector? e)
              (recur (-assoc ret (-nth e 0) (-nth e 1)) (next es))
              (throw (js/Error. "conj on a map takes map entries or seqables of map entries"))))))))
  IKVReduce
  (-kv-reduce [coll f init] (-kv-reduce m f init))
  IReduce
  (-reduce [coll f] (-reduce m f))
  (-reduce [coll f start] (-reduce m f start))
  IEquiv
  (-equiv [o other] (-equiv m other))
  IHash
  (-hash [o] (-hash m))
  IIterable
  (-iterator [coll] (-iterator m)))

(es6-iterable IndexedPersistentMap)