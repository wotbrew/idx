(ns com.wotbrew.idx-test
  (:require [clojure.test :refer :all])
  (:require [com.wotbrew.idx :refer :all]))

(deftest indexed-vector-test
  (let [v (vec (range 100))]

    (is (vector? (auto-idx v)))
    (is (identical? v (unwrap (auto-idx v))))
    (is (= {:foo 42} (meta (auto-idx (with-meta v {:foo 42})))))

    (let [v2 (auto-idx v)]
      (is (= v2 v))
      (is (= (conj v -1) (conj v2 -1)))
      (is (= (assoc v 0 100) (assoc v2 0 100)))
      (is (= (count v) (count v2)))
      (is (= (seq v) (seq v2)))

      (is (= (filter even? v) (sort (group v2 (pred even?)))))
      (is (sequential? (group v2 (pred even?))))
      (is (identical? (-get-index v2 (pred even?) :idx/hash) (-get-index v2 (pred even?) :idx/hash)))
      (is (= (filter even? v) (sort (group v2 (pred even?)))))
      (is (= (sort (filter even? (conj v 12))) (sort (group (conj v2 12) (pred even?)))))

      (is (= 4 (identify v2 identity 4)))
      (is (= 4 (identify v2 inc 5)))

      (is (= (filter even? (pop v)) (sort (group (pop v2) (pred even?)))))
      (is (= (pop v) (pop v2)))
      (is (= (peek v) (peek v2))))))

(deftest indexed-set-test
  (let [s (set (range 100))]

    (is (set? (auto-idx s)))
    (is (identical? s (unwrap (auto-idx s))))
    (is (= {:foo 42} (meta (auto-idx (with-meta s {:foo 42})))))

    (let [s2 (auto-idx s)]
      (is (= s2 s))
      (is (= (conj s -1) (conj s2 -1)))
      (is (= (count s) (count s2)))
      (is (= (seq s) (seq s2)))

      (is (= (sort (filter even? s)) (sort (group s2 (pred even?)))))
      (is (sequential? (group s2 (pred even?))))
      (is (identical? (-get-index s2 (pred even?) :idx/hash) (-get-index s2 (pred even?) :idx/hash)))
      (is (= (sort (filter even? s)) (sort (group s2 (pred even?)))))
      (is (= (sort (filter even? (conj s 12))) (sort (group (conj s2 12) (pred even?)))))

      (is (= 4 (identify s2 identity 4)))
      (is (= 4 (identify s2 inc 5)))

      (is (= (sort (filter even? (disj s 99))) (sort (group (disj s2 99) (pred even?))))))) )

(deftest indexed-map-test
  (let [m (zipmap (range 100) (range 100))]

    (is (map? (auto-idx m)))
    (is (identical? m (unwrap (auto-idx m))))
    (is (= {:foo 42} (meta (auto-idx (with-meta m {:foo 42})))))

    (let [m2 (auto-idx m)]
      (is (= m2 m))
      (is (= (conj m {-1 -1}) (conj m2 {-1 -1})))
      (is (= (count m) (count m2)))
      (is (= (seq m) (seq m2)))

      (is (= (sort (filter even? (vals m))) (sort (group m2 (pred even?)))))
      (is (sequential? (group m2 (pred even?))))
      (is (identical? (-get-index m2 even? :idx/hash) (-get-index m2 even? :idx/hash)))
      (is (= (sort (filter even? (vals m))) (sort (group m2 (pred even?)))))
      (is (= (sort (filter even? (vals (assoc m 12 12)))) (sort (group (assoc m2 12 12) (pred even?)))))

      (is (= 4 (identify m2 identity 4)))
      (is (= 4 (identify m2 inc 5)))

      (is (= (sort (filter even? (vals (dissoc m 99)))) (sort (group (dissoc m2 99) (pred even?))))))))

(deftest path-test
  (let [v (auto-idx [{:foo {:bar 42}} {:foo {:bar 43}}])]
    (is (= {:foo {:bar 42}} (identify v (path :foo :bar) 42)))
    (is (= [{:foo {:bar 43}}] (group v (path :foo :bar) 43)))))

(deftest match-test
  (let [v (auto-idx [{:foo 42,:bar 43, :baz 45} {:foo 46, :bar 47, :baz 48}])]
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo 42, :bar 43))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo 42))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo (pred even?) :bar 43))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo (match number? true odd? false) :bar 43))))
    (is (= {:foo 42, :bar 43 :baz 45} (identify v (match :foo 42, :baz 45, :bar 43))))
    (is (nil? (identify v (match :foo -1))))))

(deftest as-key-test
  (is (= {even? 4} (identify [{even? 4} {even? 5}] (as-key even?) 4)))
  (is (= {even? 4} (identify (auto-idx [{even? 4} {even? 5}]) (as-key even?) 4))))

(deftest idx-test
  (is (= {42 {0 0} 43 {1 1}} (-get-index (idx [{:foo 42} {:foo 43}] :foo :idx/hash) :foo :idx/hash)))
  (is (= {42 0 43 1} (-get-index (idx [{:foo 42} {:foo 43}] :foo :idx/unique) :foo :idx/unique)))
  (is (= {42 {0 0} 43 {1 1}} (-get-index (idx [{:foo 42} {:foo 43}] :foo :idx/sort) :foo :idx/sort)))
  (is (sorted? (-get-index (idx [{:foo 42} {:foo 43}] :foo :idx/sort) :foo :idx/sort))))

(deftest replace-by-test
  (is (= [42 43] (replace-by [41 43] identity 41 42)))
  (is (= [42 43] (replace-by (auto-idx [41 43]) identity 41 42))))
