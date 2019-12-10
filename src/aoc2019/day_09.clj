(ns aoc2019.day-09
  (:require [aoc2019.intcodes :as ic]
            [clojure.core.async :as a]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def test-1 [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
(def test-2 [1102,34915192,34915192,7,4,7,99,0])
(def test-3 [104,1125899906842624,99])

(def puzzle-input
  (let [data (slurp (io/resource "input-09.txt"))]
    (->> (s/split data #",")
         (map s/trim)
         (mapv #(Long/parseLong %)))))

(defn verify []
  (assert (= [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
             (ic/run-prg!! [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] [])))
  (assert (= 1125899906842624
             (first (ic/run-prg!! [104,1125899906842624,99] []))))
  ;; Part 1
  (assert (= [2316632620] (ic/run-prg!! puzzle-input [1])))
  ;; Part 2
  (assert (= [78869] (ic/run-prg!! puzzle-input [2])))
  "Ok")



