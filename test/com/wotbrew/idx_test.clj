(ns com.wotbrew.idx-test
  (:require [clojure.test :refer :all])
  (:require [com.wotbrew.idx :refer :all]))

(deftest indexed-vector-test
  (let [v (vec (range 100))]

    (is (vector? (idx v)))
    (is (identical? v (unwrap (idx v))))
    (is (= {:foo 42} (meta (idx (with-meta v {:foo 42})))))

    (let [v2 (idx v)]
      (is (= v2 v))
      (is (= (conj v -1) (conj v2 -1)))
      (is (= (assoc v 0 100) (assoc v2 0 100)))
      (is (= (count v) (count v2)))
      (is (= (seq v) (seq v2)))

      (is (= (filter even? v) (sort (group v2 even?))))
      (is (sequential? (group v2 even?)))
      (is (identical? (-get-eq v2 even?) (-get-eq v2 even?)))
      (is (= (filter even? v) (sort (group v2 even?))))
      (is (= (sort (filter even? (conj v 12))) (sort (group (conj v2 12) even?))))

      (is (= 4 (identify v2 identity 4)))
      (is (= 4 (identify v2 inc 5)))
      (is (= 4 (identify v2 (prop #(+ % 2)) 6)))

      (is (= (filter even? (pop v)) (sort (group (pop v2) even?))))
      (is (= (pop v) (pop v2)))
      (is (= (peek v) (peek v2))))))

(deftest indexed-set-test
  (let [s (set (range 100))]

    (is (set? (idx s)))
    (is (identical? s (unwrap (idx s))))
    (is (= {:foo 42} (meta (idx (with-meta s {:foo 42})))))

    (let [s2 (idx s)]
      (is (= s2 s))
      (is (= (conj s -1) (conj s2 -1)))
      (is (= (count s) (count s2)))
      (is (= (seq s) (seq s2)))

      (is (= (sort (filter even? s)) (sort (group s2 even?))))
      (is (sequential? (group s2 even?)))
      (is (identical? (-get-eq s2 even?) (-get-eq s2 even?)))
      (is (= (sort (filter even? s)) (sort (group s2 even?))))
      (is (= (sort (filter even? (conj s 12))) (sort (group (conj s2 12) even?))))

      (is (= 4 (identify s2 identity 4)))
      (is (= 4 (identify s2 inc 5)))
      (is (= 4 (identify s2 (prop #(+ % 2)) 6)))

      (is (= (sort (filter even? (disj s 99))) (sort (group (disj s2 99) even?)))))) )

(deftest indexed-map-test
  (let [m (zipmap (range 100) (range 100))]

    (is (map? (idx m)))
    (is (identical? m (unwrap (idx m))))
    (is (= {:foo 42} (meta (idx (with-meta m {:foo 42})))))

    (let [m2 (idx m)]
      (is (= m2 m))
      (is (= (conj m {-1 -1}) (conj m2 {-1 -1})))
      (is (= (count m) (count m2)))
      (is (= (seq m) (seq m2)))

      (is (= (sort (filter even? (vals m))) (sort (group m2 even?))))
      (is (sequential? (group m2 even?)))
      (is (identical? (-get-eq m2 even?) (-get-eq m2 even?)))
      (is (= (sort (filter even? (vals m))) (sort (group m2 even?))))
      (is (= (sort (filter even? (vals (assoc m 12 12)))) (sort (group (assoc m2 12 12) even?))))

      (is (= 4 (identify m2 identity 4)))
      (is (= 4 (identify m2 inc 5)))
      (is (= 4 (identify m2 (prop #(+ % 2)) 6)))

      (is (= (sort (filter even? (vals (dissoc m 99)))) (sort (group (dissoc m2 99) even?)))))))

(deftest path-test
  (let [v (idx [{:foo {:bar 42}} {:foo {:bar 43}}])]
    (is (= {:foo {:bar 42}} (identify v (path :foo :bar) 42)))
    (is (= [{:foo {:bar 43}}] (group v (path :foo :bar) 43)))))

(deftest match-test
  (let [v (idx [{:foo 42,:bar 43, :baz 45} {:foo 46, :bar 47, :baz 48}])]
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo 42, :bar 43))))
    (is (= {:foo 42,:bar 43, :baz 45} (identify v (match :foo 42))))
    (is (= {:foo 42, :bar 43 :baz 45} (identify v (match :foo 42, :baz 45, :bar 43))))
    (is (nil? (identify v (match :foo -1))))))