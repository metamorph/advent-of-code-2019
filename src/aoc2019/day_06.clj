(ns aoc2019.day-06
  (:require [aoc2019.util :as util]))

(def puzzle-input (util/input-lines "input-06.txt"))
(def test-input (util/input-lines "test-06.txt"))
(def test-input-2 (util/input-lines "test-06-2.txt"))

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

(defn find-common-orbit [p p']
  (last (last (take-while (partial apply =) (map vector p p')))))

(defn solve-2 [data]
  (let [my-path  (orbiters data :YOU)
        san-path (orbiters data :SAN)
        common   (find-common-orbit my-path san-path)
        my-dist  (count (take-while #(not= common %) (reverse my-path)))
        san-dist (count (take-while #(not= common %) (reverse san-path)))]
    (+ my-dist san-dist)))

(defn verify []
  (assert (= 194721 (solve-1 (input->data puzzle-input))))
  (assert (= 316 (solve-2 (input->data puzzle-input))))
  "Ok!")


