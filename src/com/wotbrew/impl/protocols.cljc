(ns com.wotbrew.impl.protocols)

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