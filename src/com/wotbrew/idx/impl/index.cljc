(ns com.wotbrew.idx.impl.index
  (:require [com.wotbrew.idx.impl.protocols :as p]))

;; for eq/sorted leaves we use maps rather than sets so
;; we get the PersistentArrayMap optimisation when small.

(defn create-eq-from-associative
  [m p]
  (let [rf (fn [m id v]
             (let [ival (p/-property p v)]
               (assoc! m ival (assoc (get m ival {}) id v))))]
    (persistent! (reduce-kv rf (transient {}) m))))

(defn create-eq-from-elements
  [coll p]
  (let [rf (fn [m [id v]]
             (let [ival (p/-property p v)]
               (assoc! m ival (assoc (get m ival {}) id v))))]
    (persistent! (reduce rf (transient {}) (p/-id-element-pairs coll)))))

(defn create-uniq-from-associative
  [m p]
  (let [rf (fn [m id v]
             (let [ival (p/-property p v)]
               (assoc! m ival id)))]
    (persistent! (reduce-kv rf (transient {}) m))))

(defn create-unique-from-elements
  [coll p]
  (let [rf (fn [m [id v]]
             (let [ival (p/-property p v)]
               (assoc! m ival id)))]
    (persistent! (reduce rf (transient {}) (p/-id-element-pairs coll)))))

(defn create-sorted-from-associative
  [m p]
  (let [rf (fn [m id v]
             (let [ival (p/-property p v)]
               (assoc m ival (assoc (get m ival {}) id v))))]
    (reduce-kv rf (sorted-map) m)))

(defn create-sorted-from-elements
  [coll p]
  (let [rf (fn [m [id v]]
             (let [ival (p/-property p v)]
               (assoc m ival (assoc (get m ival {}) id v))))]
    (reduce rf (sorted-map) (p/-id-element-pairs coll))))

(defn add-eq
  ([eq id element]
   (reduce-kv
     (fn [eq p i]
       (let [v (p/-property p element)
             nset (i v {})
             nset (assoc nset id element)
             i (assoc i v nset)]
         (assoc eq p i)))
     eq
     eq))
  ([eq id old-element element]
   (reduce-kv
     (fn [eq p i]
       (let [ov (p/-property p old-element)
             v (p/-property p element)]
         (if (identical? ov v)
           eq
           (let [oset (i ov {})
                 oset (dissoc oset id)
                 i (if (empty? oset) (dissoc i ov) (assoc i ov oset))

                 nset (get i v {})
                 nset (assoc nset id element)
                 i (assoc i v nset)]
             (assoc eq p i)))))
     eq
     eq)))

(defn del-eq [eq id old-element]
  (reduce-kv
    (fn [eq p i]
      (let [ov (p/-property p old-element)]
        (let [oset (i ov {})
              oset (dissoc oset id)
              i (if (empty? oset) (dissoc i ov) (assoc i ov oset))]
          (if (empty? i)
            (dissoc eq p)
            (assoc eq p i)))))
    eq
    eq))

(defn add-uniq
  ([unq id element]
   (reduce-kv
     (fn [unq p i]
       (let [v (p/-property p element)
             i (assoc i v id)]
         (assoc unq p i)))
     unq
     unq))
  ([unq id old-element element]
   (reduce-kv
     (fn [unq p i]
       (let [ov (p/-property p old-element)
             v (p/-property p element)]
         (cond
           (and (identical? id (i v)) (identical? ov v)) unq
           (identical? ov v) (assoc unq p (assoc i v id))
           :else
           (let [i (dissoc i ov)
                 i (assoc i v id)]
             (assoc unq p i)))))
     unq
     unq)))

(defn del-uniq [unq old-element]
  (reduce-kv
    (fn [unq p i]
      (let [ov (p/-property p old-element)]
        (let [i (dissoc i ov)]
          (if (empty? i)
            (dissoc unq p)
            (assoc unq p i)))))
    unq
    unq))

(defn add-sorted
  ([srt id element]
   (reduce-kv
     (fn [srt p i]
       (let [v (p/-property p element)
             nset (i v {})
             nset (assoc nset id element)
             i (assoc i v nset)]
         (assoc srt p i)))
     srt
     srt))
  ([srt id old-element element]
   (reduce-kv
     (fn [srt p i]
       (let [ov (p/-property p old-element)
             v (p/-property p element)]
         (if (identical? ov v)
           srt
           (let [oset (get i ov {})
                 oset (dissoc oset id)
                 i (if (empty? oset) (dissoc i ov) (assoc i ov oset))

                 nset (get i v {})
                 nset (assoc nset id element)
                 i (assoc i v nset)]
             (assoc srt p i)))))
     srt
     srt)))

(defn del-sorted [srt id old-element]
  (reduce-kv
    (fn [srt p i]
      (let [ov (p/-property p old-element)]
        (let [oset (i ov {})
              oset (dissoc oset id)
              i (if (empty? oset) (dissoc i ov) (assoc i ov oset))]
          (if (empty? i)
            (dissoc srt p)
            (assoc srt p i)))))
    srt
    srt))