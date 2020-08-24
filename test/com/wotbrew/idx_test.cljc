(ns com.wotbrew.idx-test
  (:require [clojure.test :refer [deftest is]]
            [com.wotbrew.idx :refer [lookup unwrap auto identify pred match index path replace-by as-key ascending descending]]
            [com.wotbrew.impl.protocols :as p]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check :as tc]))

(deftest indexed-vector-test
  (let [v (vec (range 100))]

    (is (vector? (auto v)))
    (is (identical? v (unwrap (auto v))))
    (is (= {:foo 42} (meta (auto (with-meta v {:foo 42})))))

    (let [v2 (auto v)]
      (is (= v2 v))
      (is (= (conj v -1) (conj v2 -1)))
      (is (= (assoc v 0 100) (assoc v2 0 100)))
      (is (= (count v) (count v2)))
      (is (= (seq v) (seq v2)))

      (is (= (filter even? v) (sort (lookup v2 (pred even?)))))
      (is (sequential? (lookup v2 (pred even?))))
      (is (identical? (p/-get-index v2 (pred even?) :idx/hash) (p/-get-index v2 (pred even?) :idx/hash)))
      (is (= (filter even? v) (sort (lookup v2 (pred even?)))))
      (is (= (sort (filter even? (conj v 12))) (sort (lookup (conj v2 12) (pred even?)))))

      (is (= 4 (identify v2 identity 4)))
      (is (= 4 (identify v2 inc 5)))

      (is (= (filter even? (pop v)) (sort (lookup (pop v2) (pred even?)))))
      (is (= (pop v) (pop v2)))
      (is (= (peek v) (peek v2))))))

(deftest indexed-set-test
  (let [s (set (range 100))]

    (is (set? (auto s)))
    (is (identical? s (unwrap (auto s))))
    (is (= {:foo 42} (meta (auto (with-meta s {:foo 42})))))

    (let [s2 (auto s)]
      (is (= s2 s))
      (is (= (conj s -1) (conj s2 -1)))
      (is (= (count s) (count s2)))
      (is (= (seq s) (seq s2)))

      (is (= (sort (filter even? s)) (sort (lookup s2 (pred even?)))))
      (is (sequential? (lookup s2 (pred even?))))
      (is (identical? (p/-get-index s2 (pred even?) :idx/hash) (p/-get-index s2 (pred even?) :idx/hash)))
      (is (= (sort (filter even? s)) (sort (lookup s2 (pred even?)))))
      (is (= (sort (filter even? (conj s 12))) (sort (lookup (conj s2 12) (pred even?)))))

      (is (= 4 (identify s2 identity 4)))
      (is (= 4 (identify s2 inc 5)))

      (is (= (sort (filter even? (disj s 99))) (sort (lookup (disj s2 99) (pred even?))))))))

(deftest indexed-map-test
  (let [m (zipmap (range 100) (range 100))]

    (is (map? (auto m)))
    (is (identical? m (unwrap (auto m))))
    (is (= {:foo 42} (meta (auto (with-meta m {:foo 42})))))

    (let [m2 (auto m)]
      (is (= m2 m))
      (is (= (conj m {-1 -1}) (conj m2 {-1 -1})))
      (is (= (count m) (count m2)))
      (is (= (seq m) (seq m2)))

      (is (= (sort (filter even? (vals m))) (sort (lookup m2 (pred even?)))))
      (is (sequential? (lookup m2 (pred even?))))
      (is (identical? (p/-get-index m2 even? :idx/hash) (p/-get-index m2 even? :idx/hash)))
      (is (= (sort (filter even? (vals m))) (sort (lookup m2 (pred even?)))))
      (is (= (sort (filter even? (vals (assoc m 12 12)))) (sort (lookup (assoc m2 12 12) (pred even?)))))

      (is (= 4 (identify m2 identity 4)))
      (is (= 4 (identify m2 inc 5)))

      (is (= (sort (filter even? (vals (dissoc m 99)))) (sort (lookup (dissoc m2 99) (pred even?))))))))

(deftest path-test
  (let [v (auto [{:foo {:bar 42}} {:foo {:bar 43}}])]
    (is (= {:foo {:bar 42}} (identify v (path :foo :bar) 42)))
    (is (= [{:foo {:bar 43}}] (lookup v (path :foo :bar) 43)))))

(deftest match-test
  (let [v (auto [{:foo 42,:bar 43, :baz 45} {:foo 46, :bar 47, :baz 48}])]
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo 42, :bar 43))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo 42))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo (pred even?) :bar 43))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo (match number? true odd? false) :bar 43))))
    (is (= {:foo 42, :bar 43 :baz 45} (identify v (match :foo 42, :baz 45, :bar 43))))
    (is (nil? (identify v (match :foo 100))))))

(deftest as-key-test
  (is (= {even? 4} (identify [{even? 4} {even? 5}] (as-key even?) 4)))
  (is (= {even? 4} (identify (auto [{even? 4} {even? 5}]) (as-key even?) 4))))

(deftest idx-test
  (is (= {42 {0 {:foo 42}} 43 {1 {:foo 43}}} (p/-get-index (index [{:foo 42} {:foo 43}] :foo :idx/hash) :foo :idx/hash)))
  (is (= {42 0 43 1} (p/-get-index (index [{:foo 42} {:foo 43}] :foo :idx/unique) :foo :idx/unique)))
  (is (= {42 {0 {:foo 42}} 43 {1 {:foo 43}}} (p/-get-index (index [{:foo 42} {:foo 43}] :foo :idx/sort) :foo :idx/sort)))
  (is (sorted? (p/-get-index (index [{:foo 42} {:foo 43}] :foo :idx/sort) :foo :idx/sort))))

(deftest replace-by-test
  (is (= [42 43] (replace-by [41 43] identity 41 42)))
  (is (= [42 43] (replace-by (auto [41 43]) identity 41 42))))

(def index-pair
  (gen/elements
    [[identity :idx/hash]
     [identity :idx/unique]
     [hash :idx/sort]]))

;; vectors

(def i-vec-pair
  (gen/bind
    (gen/vector gen/any-printable-equatable 1 100)
    (fn [v]
      (gen/tuple (gen/choose 0 (dec (count v))) (gen/return v)))))

(defn same-vec? [v1 v2]
  (and (= v1 v2)
       (vector? v1)
       (vector? v2)
       (= (peek v1) (peek v2))
       (= (count v1) (count v2))
       (= (first v1) (first v2))
       (= (hash v1) (hash v2))
       (= (str v1) (str v2))
       (= (pr-str v1) (pr-str v2))))

(def vec-equality-holds-through-conj
  (prop/for-all [v (gen/vector gen/any-printable-equatable)
                 e gen/any-printable-equatable
                 indexes (gen/vector index-pair)]
    (same-vec? (conj v e) (conj (apply index v (mapcat identity indexes)) e))))

(def vec-equality-holds-through-pop
  (prop/for-all [v (gen/vector gen/any-printable-equatable 1 100)
                 indexes (gen/vector index-pair)]
    (same-vec? (pop v) (pop (apply index v (mapcat identity indexes))))))

(def vec-equality-holds-through-assoc
  (prop/for-all [[i v] i-vec-pair
                 e gen/any-printable-equatable
                 indexes (gen/vector index-pair)]
    (same-vec? (assoc v i e) (assoc (apply index v (mapcat identity indexes)) i e))))

(def vec-identity-lookup-equality
  (prop/for-all [v (gen/vector gen/any-printable-equatable)
                 e gen/any-printable-equatable]
    (let [v (index v identity :idx/unique)
          v (conj v e)]
      (is (= e (identify v identity e))))))

(def vec-hash-membership
  (prop/for-all [v (gen/vector gen/any-printable-equatable)
                 e gen/any-printable-equatable]
    (let [v (index v hash :idx/hash)
          v (conj v e)]
      (is (contains? (set (lookup v hash (hash e))) e)))))

(def vec-ascending-same-as-sort
  (prop/for-all [v (gen/vector gen/pos-int)]
    (let [v (index v identity :idx/sort)]
      (is (= (sort v) (ascending v identity >= 0))))))

(def vec-descending-same-as-reverse-sort
  (prop/for-all [v (gen/vector gen/pos-int)]
    (let [v (index v identity :idx/sort)]
      (is (= (reverse (sort v)) (descending v identity >= 0))))))

;; maps

(def key-map-pair
  (gen/bind
    (gen/map gen/any-printable-equatable gen/any-printable-equatable {:min-elements 1})
    (fn [m]
      (gen/tuple (gen/elements (keys m)) (gen/return m)))))

(defn same-map? [v1 v2]
  (and (= v1 v2)
       (map? v1)
       (map? v2)
       (= (seq v1) (seq v2))
       (= (count v1) (count v2))
       (= (first v1) (first v2))
       (= (hash v1) (hash v2))
       (= (str v1) (str v2))
       (= (pr-str v1) (pr-str v2))))

(def map-equality-holds-through-conj
  (prop/for-all [m (gen/map gen/any-printable-equatable gen/any-printable-equatable)
                 k gen/any-printable-equatable
                 v gen/any-printable-equatable
                 indexes (gen/vector index-pair)]
    (same-map? (conj m {k v}) (conj (apply index m (mapcat identity indexes)) {k v}))))

(def map-equality-holds-through-dissoc
  (prop/for-all [[k m] key-map-pair
                 indexes (gen/vector index-pair)]
    (same-map? (dissoc m k) (dissoc (apply index m (mapcat identity indexes)) k))))

(def map-equality-holds-through-assoc
  (prop/for-all [[k m] key-map-pair
                 e gen/any-printable-equatable
                 indexes (gen/vector index-pair)]
    (same-map? (assoc m k e) (assoc (apply index m (mapcat identity indexes)) k e))))

(def map-identity-lookup-equality
  (prop/for-all [m (gen/map gen/any-printable-equatable gen/any-printable-equatable)
                 e gen/any-printable-equatable]
    (let [m (index m identity :idx/unique)
          m (conj m {e e})]
      (is (= e (identify m identity e))))))

(def map-hash-membership
  (prop/for-all [m (gen/map gen/any-printable-equatable gen/any-printable-equatable)
                 e gen/any-printable-equatable]
    (let [m (index m hash :idx/hash)
          m (conj m {e e})]
      (is (contains? (set (lookup m hash (hash e))) e)))))

(def map-ascending-same-as-sort
  (prop/for-all [m (gen/map gen/any-printable-equatable gen/pos-int)]
    (let [m (index m identity :idx/sort)]
      (is (= (sort (vals m)) (ascending m identity >= 0))))))

(def map-descending-same-as-reverse-sort
  (prop/for-all [m (gen/map gen/any-printable-equatable gen/pos-int)]
    (let [m (index m identity :idx/sort)]
      (is (= (reverse (sort (vals m))) (descending m identity >= 0))))))


(def key-set-pair
  (gen/bind
    (gen/set gen/any-printable-equatable {:min-elements 1})
    (fn [s]
      (gen/tuple (gen/elements s) (gen/return s)))))

(defn same-set? [v1 v2]
  (and (= v1 v2)
       (set? v1)
       (set? v2)
       (= (seq v1) (seq v2))
       (= (count v1) (count v2))
       (= (first v1) (first v2))
       (= (hash v1) (hash v2))
       (= (str v1) (str v2))
       (= (pr-str v1) (pr-str v2))))

(def set-equality-holds-through-conj
  (prop/for-all [s (gen/set gen/any-printable-equatable)
                 v gen/any-printable-equatable
                 indexes (gen/vector index-pair)]
    (same-map? (conj s v) (conj (apply index s (mapcat identity indexes)) v))))

(def set-equality-holds-through-disj
  (prop/for-all [[k s] key-set-pair
                 indexes (gen/vector index-pair)]
    (same-map? (disj s k) (disj (apply index s (mapcat identity indexes)) k))))

(def set-identity-lookup-equality
  (prop/for-all [s (gen/set gen/any-printable-equatable)
                 e gen/any-printable-equatable]
    (let [m (index s identity :idx/unique)
          m (conj s e)]
      (is (= e (identify m identity e))))))

(def set-hash-membership
  (prop/for-all [s (gen/set gen/any-printable-equatable)
                 e gen/any-printable-equatable]
    (let [s (index s hash :idx/hash)
          s (conj s e)]
      (is (contains? (set (lookup s hash (hash e))) e)))))

(def set-ascending-same-as-sort
  (prop/for-all [s (gen/set gen/pos-int)]
    (let [s (index s identity :idx/sort)]
      (is (= (sort s) (ascending s identity >= 0))))))

(def set-descending-same-as-reverse-sort
  (prop/for-all [s (gen/set gen/pos-int)]
    (let [s (index s identity :idx/sort)]
      (is (= (reverse (sort s)) (descending s identity >= 0))))))


(comment
  (clojure.test/run-tests)

  (let [vec-props
        '[{:p vec-equality-holds-through-conj}
          {:p vec-equality-holds-through-pop}
          {:p vec-equality-holds-through-assoc}
          {:p vec-identity-lookup-equality}
          {:p vec-hash-membership}
          {:p vec-ascending-same-as-sort}
          {:p vec-descending-same-as-reverse-sort}]

        map-props
        '[{:p map-equality-holds-through-assoc}
          {:p map-equality-holds-through-conj}
          {:p map-equality-holds-through-dissoc}
          {:p map-identity-lookup-equality}
          {:p map-hash-membership}
          {:p map-ascending-same-as-sort}
          {:p map-descending-same-as-reverse-sort}]

        set-props
        '[{:p set-equality-holds-through-conj}
          {:p set-equality-holds-through-disj}
          {:p set-hash-membership}
          {:p set-identity-lookup-equality}
          {:p set-ascending-same-as-sort}
          {:p set-descending-same-as-reverse-sort}]

        noop-idx false
        ntests 100]

    (with-redefs [index (if noop-idx (fn [c & args] c) index)]
      (doseq [prop (concat vec-props
                           map-props
                           set-props)]
        (println "Testing property" (:p prop))
        (println "=>")
        (prn (tc/quick-check ntests @(resolve (:p prop))))
        (println "")))))