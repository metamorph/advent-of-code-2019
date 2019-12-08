(ns aoc2019.day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def img-size [25 6])

(def puzzle-input (->> (io/resource "input-08.txt")
                      slurp
                      (s/trim)
                      (map #(Byte/parseByte (str %)))))

(defn stacked-layers [input [width height :as size]]
  (let [layers (partition (* width height) input)]
    (apply map vector layers)))

(defn eval-pixel [ns] (first (filter #(not= 2 %) ns)))

(defn img-matrix [input [width height :as size]]
  (let [pixels (map eval-pixel (stacked-layers input size))]
    (partition width pixels)))

(defn print-matrix [matrix]
  (doseq [line matrix]
    (println (s/join
              (map #(if (= % 1) "#" " ") line)))))

(defn verify []
  (print-matrix (img-matrix puzzle-input img-size)))



