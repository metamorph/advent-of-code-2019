(ns aoc2019.util
  (:require [clojure.java.io :as io]))

(defn input-lines [resource]
  (line-seq (io/reader (io/resource resource))))
