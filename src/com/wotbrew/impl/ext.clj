(ns com.wotbrew.impl.ext
  (:require [com.wotbrew.impl.protocols :as p]
            [com.wotbrew.impl.map :as imap]
            [com.wotbrew.impl.vector :as ivec]
            [com.wotbrew.impl.set :as iset])
  (:import (clojure.lang Fn Var Keyword IPersistentMap IPersistentVector IPersistentSet)))

(extend-protocol p/Idx
  nil
  (-rewrap [coll auto] nil)
  (-get-index [coll p kind] nil)
  (-del-index [coll p kind] nil)
  (-add-index [coll p kind] (-> (p/-wrap coll false) (p/-add-index p kind)))
  (-elements [coll] nil)
  (-id-element-pairs [coll] nil)
  Object
  (-rewrap [coll auto] coll)
  (-get-index [coll p kind] nil)
  (-del-index [coll p kind] coll)
  (-add-index [coll p kind] (-> (p/-wrap coll false) (p/-add-index p kind)))
  (-elements [coll] (p/-elements (p/-wrap coll false)))
  (-id-element-pairs [coll] (p/-id-element-pairs (p/-wrap coll false))))

(extend-protocol p/Property
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

(extend-protocol p/Wrap
  IPersistentMap
  (-wrap [this auto] (imap/->IndexedPersistentMap this {} {} {} auto))
  IPersistentVector
  (-wrap [this auto] (ivec/->IndexedPersistentVector this {} {} {} auto))
  IPersistentSet
  (-wrap [this auto] (iset/->IndexedPersistentSet this {} {} {} auto))
  nil
  (-wrap [this auto] (p/-wrap [] auto))
  Object
  (-wrap [this auto] (p/-wrap (with-meta (vec this) (meta this)) auto)))

(extend-protocol p/Unwrap
  nil
  (-unwrap [coll] coll)
  Object
  (-unwrap [coll] coll))