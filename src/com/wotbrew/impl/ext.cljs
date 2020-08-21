(ns com.wotbrew.impl.ext
  (:require [com.wotbrew.impl.protocols :as p]
            [com.wotbrew.impl.map :as imap]
            [com.wotbrew.impl.vector :as ivec]
            [com.wotbrew.impl.set :as iset]))

(extend-protocol
  p/Idx
  nil
  (-rewrap [coll auto] nil)
  (-get-index [coll p kind] nil)
  (-del-index [coll p kind] nil)
  (-add-index [coll p kind] (-> (p/-wrap coll false) (p/-add-index p kind)))
  (-elements [coll] nil)
  (-id-element-pairs [coll] nil)
  default
  (-rewrap [coll auto] coll)
  (-get-index [coll p kind] nil)
  (-del-index [coll p kind] coll)
  (-add-index [coll p kind] (-> (p/-wrap coll false) (p/-add-index p kind)))
  (-elements [coll] (p/-elements (p/-wrap coll false)))
  (-id-element-pairs [coll] (p/-id-element-pairs (p/-wrap coll false))))

(extend-protocol p/Property
  default
  (-property [this element] (get element this))
  function
  (-property [this element] (this element))
  nil
  (-property [this element] nil)
  Fn
  (-property [this element] (this element))
  Keyword
  (-property [this element] (this element)))

(extend-protocol p/Wrap
  nil
  (-wrap [this auto] (p/-wrap [] auto))
  default
  (-wrap [this auto] (p/-wrap (with-meta (vec this) (meta this)) auto))

  PersistentArrayMap
  (-wrap [this auto] (imap/->IndexedPersistentMap this nil nil nil auto))
  PersistentHashMap
  (-wrap [this auto] (imap/->IndexedPersistentMap this nil nil nil auto))
  PersistentTreeMap
  (-wrap [this auto] (imap/->IndexedPersistentMap this nil nil nil auto))
  PersistentVector
  (-wrap [this auto] (ivec/->IndexedPersistentVector this nil nil nil auto))
  Subvec
  (-wrap [this auto] (ivec/->IndexedPersistentVector this nil nil nil auto))
  PersistentHashSet
  (-wrap [this auto] (iset/->IndexedPersistentSet this nil nil nil auto))
  PersistentTreeSet
  (-wrap [this auto] (iset/->IndexedPersistentSet this nil nil nil auto)))

(extend-protocol p/Unwrap
  nil
  (-unwrap [coll] coll)
  default
  (-unwrap [coll] coll))