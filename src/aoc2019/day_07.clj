(ns aoc2019.day-07
  (:require [aoc2019.day-05 :refer [run run!!]]
            [clojure.math.combinatorics :as comb]
            [aoc2019.util :refer [input-lines]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.async :as a]))

(def puzzle-input
  (let [data (slurp (io/resource "input-07.txt"))]
    (->> (s/split data #",")
         (map s/trim)
         (mapv #(Integer/parseInt %)))))

(defn signal [prg phase-settings seed]
  (loop [input seed
         phases phase-settings]
    (if (seq phases)
      (let [output (run!! prg (list (first phases) input))]
        (recur output (rest phases)))
      input)))

(defn max-output [prg]
  (let [phases (comb/permutations (range 0 5))]
    (->> (map (fn [p] [p (signal prg p 0)]) phases)
         (sort-by #(* -1 (last %)))
         (first))))

(defn verify []
  (assert (= 43210 (signal [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
                           [4 3 2 1 0]
                           0)))
  (assert (= 54321 (signal [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                            101,5,23,23,1,24,23,23,4,23,99,0,0]
                           [0 1 2 3 4]
                           0)))
  (assert (= 65210 (signal [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                            1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
                           [1 0 4 3 2]
                           0)))
  (assert (= 99376 (last (max-output puzzle-input))))
  "Ok!")
