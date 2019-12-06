(ns aoc2019.day-06
  (:require [aoc2019.util :as util]))

(def puzzle-input (util/input-lines "input-06.txt"))
(def test-input (util/input-lines "test-06.txt"))

(defn input->data [lines]
  (let [re #"([^\)]*)\)([^\)]*)"]
    (into {} (for [line lines]
               (->> (re-find re line)
                    (drop 1)
                    (mapv keyword)
                    ;; Turn it around - map orbiter -> orbitee
                    reverse
                    vec)))))

(defn orbiters
  "Find all bodies that `k` orbits around."
  [m k]
  (loop [k k
         r '()]
    (if-let [k' (get m k)]
      (recur k' (conj r k'))
      r)))

(defn solve-1 [data]
  (->> (keys data)
       (map (partial orbiters data))
       (map count)
       (reduce + 0)))

(defn verify []
  (assert (= 194721 (solve-1 (input->data puzzle-input))))
  "Ok!")


