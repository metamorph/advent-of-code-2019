(ns aoc2019.day-05
  (:require [aoc2019.util :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def input
  (let [data (slurp (io/resource "input-05.txt"))]
    (->> (s/split data #",")
         (map s/trim)
         (mapv #(Integer/parseInt %)))))

(def test-input [3, 0, 4, 0, 99])

(def test-input-2 [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                   1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                   999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(defn op [n]
  (let [code (rem n 100)
        p1 (quot (rem n 1000) 100)
        p2 (quot (rem n 10000) 1000)
        p3 (quot (rem n 100000) 10000)]
    [code p1 p2 p3]))

(defn step [{:keys [pos prg inputs] :as state}]
  (let [[code m1 m2 m3] (op (first (drop pos prg)))]
    (case code
      ;; 1 - add values
      1 (let [[a b c] (take 3 (drop (inc pos) prg))
              a (if (zero? m1) (get prg a) a)
              b (if (zero? m2) (get prg b) b)]
          (-> state
              (update :pos + 4)
              (update :prg assoc c (+ a b))))
      ;; 2 - mult values
      2 (let [[a b c] (take 3 (drop (inc pos) prg))
              a (if (zero? m1) (get prg a) a)
              b (if (zero? m2) (get prg b) b)]
          (-> state
              (update :pos + 4)
              (update :prg assoc c (* a b))))
      ;; 3 - store INPUT
      3 (let [a (first (drop (inc pos) prg))]
          (-> state
              (update :pos + 2)
              (update :prg assoc a (first inputs))
              (update :inputs rest)))
      ;; 4 - write OUTPUT
      4 (let [a (first (drop (inc pos) prg))]
          (-> state
              (update :outputs conj (get prg a))
              (update :pos + 2)))
      ;; 5 - jump-if-true [>0, new-pos], else ignore
      5 (let [[a b] (take 2 (drop (inc pos) prg))
              a (if (zero? m1) (get prg a) a)
              b (if (zero? m2) (get prg b) b)]
          (if (pos? a)
            (-> state
                (assoc :pos b))
            (update state :pos + 3)))
      ;; 6 - jump-if-false [0, new-pos], else ignore
      6 (let [[a b] (take 2 (drop (inc pos) prg))
              a (if (zero? m1) (get prg a) a)
              b (if (zero? m2) (get prg b) b)]
          (if (zero? a)
            (-> state
                (assoc :pos b))
            (update state :pos + 3)))
      ;; 7 - less-than [a < b, store 1 in c, else 0]
      7 (let [[a b c] (take 3 (drop (inc pos) prg))
              a (if (zero? m1) (get prg a) a)
              b (if (zero? m2) (get prg b) b)]
          (-> state
              (update :pos + 4)
              (update :prg assoc c (if (< a b) 1 0))))
      ;; 8 - equals [a == b, store 1 in c, else 0]
      8 (let [[a b c] (take 3 (drop (inc pos) prg))
              a (if (zero? m1) (get prg a) a)
              b (if (zero? m2) (get prg b) b)]
          (-> state
              (update :pos + 4)
              (update :prg assoc c (if (= a b) 1 0))))
      ;; End-state
      99 (assoc state :halt? true))))

(defn run [prg inputs]
  (let [steps (iterate step {:pos 0
                             :halt? false
                             :prg prg
                             :inputs (seq inputs)
                             :outputs '()})]
    (:outputs (first (drop-while #(not (:halt? %)) steps)))))

(defn run-test []
  (assert (= 2845163 (first (run input (list 1)))))
  (assert (= 9436229 (first (run input (list 5)))))
  "Success!")
