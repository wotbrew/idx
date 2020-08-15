(ns com.wotbrew.idx-test
  (:require [clojure.test :refer :all])
  (:require [com.wotbrew.idx :refer :all]))

(deftest indexed-vector-test
  (let [v (vec (range 10))]

    (is (vector? (idx v)))
    (is (identical? v (unwrap (idx v))))
    (is (= {:foo 42} (meta (idx (with-meta v {:foo 42})))))

    (let [v2 (idx v)]
      (is (= v2 v))
      (is (= (conj v -1) (conj v2 -1)))
      (is (= (assoc v 0 100) (assoc v2 0 100)))

      (is (= (filter even? v) (sort (group v2 even?))))
      (is (sequential? (group v2 even?)))
      (is (identical? (-get-eq v2 even?) (-get-eq v2 even?)))
      (is (= (filter even? v) (sort (group v2 even?))))
      (is (= (filter even? (conj v 12)) (sort (group (conj v2 12) even?))))

      (is (= 4 (identify v2 identity 4)))
      (is (= 4 (identify v2 inc 5)))
      (is (= 4 (identify v2 (prop #(+ % 2)) 6)))

      (is (= (filter even? (pop v)) (sort (group (pop v2) even?))))
      (is (= (pop v) (pop v2)))
      (is (= (peek v) (peek v2))))))

(deftest indexed-set-test
  )

(deftest indexed-map-test)
