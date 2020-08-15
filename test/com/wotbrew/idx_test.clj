(ns com.wotbrew.idx-test
  (:require [clojure.test :refer :all])
  (:require [com.wotbrew.idx :refer :all]))

(deftest indexed-vector-test
  (let [v (vec (range 10))]

    (is (vector? (idx v)))
    (is (identical? v (unwrap (idx v))))
    (is (= {:foo 42} (meta (idx (with-meta v {:foo 42})))))

    (let [v2 (idx v)]
      (is (= (filter even? v) (sort (group v2 even?))))
      (is (sequential? (group v2 even?)))
      (is (identical? (-get-eq v2 even?) (-get-eq v2 even?)))
      (is (= (filter even? v) (sort (group v2 even?))))
      (is (= (filter even? (conj v 12)) (sort (group (conj v2 12) even?)))))))

(deftest indexed-set-test
  )

(deftest indexed-map-test)
