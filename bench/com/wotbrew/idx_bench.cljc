(ns com.wotbrew.idx-bench
  (:require [com.wotbrew.idx :as idx]
            [criterium.core :as c]))

(def sample (vec (shuffle (range 10000))))
(def prop #(mod % 10))

(comment

  ;; hash idx
  (c/bench (idx/index sample prop :idx/hash))

  ;; group-by
  (c/bench (group-by prop sample))

  ;; unique
  (c/bench (idx/index sample identity :idx/unique))

  ;; unique-manual
  (c/bench (persistent! (reduce-kv #(assoc! %1 %3 %2) (transient {}) sample)))

  ;; sorted
  (c/bench (idx/index sample prop :idx/sort))

  ;; get group
  (let [g (group-by prop sample)]
    (c/bench (get g 0)))

  ;; get
  (let [v (idx/auto sample)]
    (c/bench (idx/lookup v prop 0)))

  ;; identify manual
  (let [g (persistent! (reduce-kv #(assoc! %1 %3 %2) (transient {}) sample))]
    (c/bench (get g 4389)))

  ;; get
  (let [v (idx/auto sample)]
    (c/bench (idx/identify v identity 4389)))

  ;; conj 1 hash manual
  (let [m (zipmap sample sample)]
    (c/bench
      (do (conj sample 2333)
          (assoc m 2333 2333))))

  ;; conj 1 hash
  (let [v (idx/index sample identity :idx/unique)]
    (c/bench (conj v 2333)))

  ;; conj
  (let [m1 (zipmap sample sample)]
    (c/bench (assoc m1 10001 10001)))

  ;; conj 1 hash
  (let [m (idx/index (zipmap sample sample) identity :idx/unique)]
    (c/bench (assoc m 10001 10001)))

  )