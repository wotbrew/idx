(ns com.wotbrew.idx.impl.protocols)

(defprotocol Property
  "You should consider this protocol an implementation detail for now."
  (-property [this element]))

(defprotocol Predicate
  "You should consider this protocol an implementation detail for now."
  (-prop [this])
  (-predv [this]))

(defprotocol Idx
  "You should consider this protocol an implementation detail for now."
  (-rewrap [idx auto])
  (-get-eq [idx p])
  (-get-uniq [idx p])
  (-get-sort [idx p])
  (-del-index [idx p kind])
  (-add-index [idx p kind])
  (-elements [idx])
  (-id-element-pairs [idx]))

(defn -get-index [idx p kind]
  (case kind
    :idx/hash (-get-eq idx p)
    :idx/unique (-get-uniq idx p)
    :idx/sort (-get-sort idx p)))

(defprotocol Wrap
  "You should consider this protocol an implementation detail for now."
  (-wrap [coll auto]))

(defprotocol Unwrap
  "You should consider this protocol an implementation detail for now."
  (-unwrap [coll]))