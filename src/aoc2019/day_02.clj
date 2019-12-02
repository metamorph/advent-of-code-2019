(ns aoc2019.day-02
  (:require [clojure.string :as s]))

(def puzzle-input
  (mapv
   #(Integer/parseInt %)
   (s/split
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,5,23,2,23,9,27,1,5,27,31,1,9,31,35,1,35,10,39,2,13,39,43,1,43,9,47,1,47,9,51,1,6,51,55,1,13,55,59,1,59,13,63,1,13,63,67,1,6,67,71,1,71,13,75,2,10,75,79,1,13,79,83,1,83,10,87,2,9,87,91,1,6,91,95,1,9,95,99,2,99,10,103,1,103,5,107,2,6,107,111,1,111,6,115,1,9,115,119,1,9,119,123,2,10,123,127,1,127,5,131,2,6,131,135,1,135,5,139,1,9,139,143,2,143,13,147,1,9,147,151,1,151,2,155,1,9,155,0,99,2,0,14,0" #",")))

(defn read-instr [n prg]
  (take 4 (drop (* 4 n) prg)))

(defn run-prg [input]
  (loop [prg input
         n 0]
    (let [[op a b w] (read-instr n prg)
          a' (get prg a)
          b' (get prg b)]
      (if (= 99 op)
        prg
        (let [v (case op
                  1 (+ a' b')
                  2 (* a' b')
                  (throw (ex-info "Unknown op code" {:op op :iteration n})))]
          (recur (assoc prg w v) (inc n)))))))

(defn run-with [prg noun verb]
  (-> prg
      (assoc 1 noun)
      (assoc 2 verb)
      (run-prg)
      (first)))

(defn solve-1 []
  (run-with puzzle-input 12 2))

(defn solve-2 []
  (let [pairs (for [noun (range 0 100)
                    verb (range 0 100)]
                [noun verb])]
    (first (filter (fn [[noun verb]]
                     (= 19690720 (run-with puzzle-input noun verb)))
                   pairs))))

;; 0: 3101878

