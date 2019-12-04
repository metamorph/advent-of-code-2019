(ns aoc2019.day-4)

(def min-value 138307)
(def max-value 654504)
(def candidates (range min-value (inc max-value)))
(defn split-int [n] (map #(Byte/parseByte (str %)) (str n)))
(defn increasing? [ns] (apply <= ns))
(defn six-digits? [ns] (= 6 (count ns)))
(defn adjecent? [ns] (some #(<= 2 (count %)) (partition-by identity ns)))

(defn solve [ns pred]
  (->> ns
       (map split-int)
       (filter pred)
       (count)))

;; Additional predicate for part 2
(defn two-adjecent? [ns] (some #(= 2 (count %)) (partition-by identity ns)))

;; Crunching
(defn solve-1 [ns] (time (solve ns (every-pred increasing?
                                               six-digits?
                                               adjecent?))))
(defn solve-2 [ns] (time (solve ns (every-pred increasing?
                                               six-digits?
                                               adjecent?
                                               two-adjecent?))))
