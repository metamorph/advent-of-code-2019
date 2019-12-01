(ns aoc2019.day-01
  (:require [aoc2019.util :refer :all]))

(defn mass->fuel [mass]
  (-> mass
      (/ 3)
      (Math/floor)
      (- 2)
      (int)))

(defn mass->fuel-2 [mass]
  (loop [steps '()
         m mass]
    (let [fuel (mass->fuel m)]
      (if (or (zero? fuel) (neg? fuel))
        (reduce + 0 steps)
        (recur (conj steps fuel) fuel)))))

(defn solve-1 []
  (let [ms (map #(Integer/parseInt %) (input-lines "input-01.txt"))]
    (reduce + 0 (map mass->fuel ms))))

(defn solve-2 []
  (let [ms (map #(Integer/parseInt %) (input-lines "input-01.txt"))]
    (reduce + 0 (map mass->fuel-2 ms))))
