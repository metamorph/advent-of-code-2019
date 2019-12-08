(ns aoc2019.intcodes
  (:require [clojure.core.async :as a]))


(defn op
  "Split an integer into op-code and parameter types: `n -> [code p1 p2 p3]`"
  [n]
  (let [code (rem n 100)
        p1 (quot (rem n 1000) 100)
        p2 (quot (rem n 10000) 1000)
        p3 (quot (rem n 100000) 10000)]
    [code p1 p2 p3]))

(defn read-pos [prg v mode] (if (zero? mode) (get prg v) v))

(defmulti apply-op (fn [[code & _] _] code))

;; Addition
(defmethod apply-op 1 [[_ m1 m2 m3] {:keys [pos prg] :as state}]
  (let [[a b c] (take 3 (drop (inc pos) prg))]
    (-> state
        (update :pos + 4)
        (update :prg assoc c (+ (read-pos prg a m1)
                                (read-pos prg b m2))))))

;; Multiply
(defmethod apply-op 2 [[_ m1 m2 m3] {:keys [pos prg] :as state}]
  (let [[a b c] (take 3 (drop (inc pos) prg))]
    (-> state
        (update :pos + 4)
        (update :prg assoc c (* (read-pos prg a m1)
                                (read-pos prg b m2))))))

;; Read input and store it
(defmethod apply-op 3 [_ {:keys [pos prg input-fn] :as state}]
  (let [a  (first (drop (inc pos) prg))
        in (input-fn)]
    (-> state
        (update :pos + 2)
        (update :prg assoc a in))))

;; Write output
(defmethod apply-op 4 [_ {:keys [pos prg output-fn] :as state}]
  (let [a (first (drop (inc pos) prg))
        v (get prg a)]
    (output-fn v)
    (update state :pos + 2)))

;; Jump if true
(defmethod apply-op 5 [[_ m1 m2 _] {:keys [pos prg] :as state}]
  (let [[a b] (take 2 (drop (inc pos) prg))]
    (if (pos? (read-pos prg a m1))
      (assoc state :pos (read-pos prg b m2))
      (update state :pos + 3))))

;; Jump if false
(defmethod apply-op 6 [[_ m1 m2 _] {:keys [pos prg] :as state}]
  (let [[a b] (take 2 (drop (inc pos) prg))]
    (if (zero? (read-pos prg a m1))
      (assoc state :pos (read-pos prg b m2))
      (update state :pos + 3))))

;; Less-than [a < b, store 1 in c, else 0]
(defmethod apply-op 7 [[_ m1 m2 _] {:keys [pos prg] :as state}]
  (let [[a b c] (take 3 (drop (inc pos) prg))]
    (-> state
        (update :pos + 4)
        (update :prg assoc c (if (< (read-pos prg a m1) (read-pos prg b m2)) 1 0)))))

;; Equals [a == b, store 1 in c, else 0]
(defmethod apply-op 8 [[_ m1 m2 _] {:keys [pos prg] :as state}]
  (let [[a b c] (take 3 (drop (inc pos) prg))]
    (-> state
        (update :pos + 4)
        (update :prg assoc c (if (= (read-pos prg a m1) (read-pos prg b m2)) 1 0)))))

;; End state
(defmethod apply-op 99 [_ {:keys [stop-fn] :as state}]
  (stop-fn)
  (assoc state :halt? true))


(defn step [{:keys [pos prg] :as state}]
  (let [[code m1 m2 m3 :as pfx] (op (first (drop pos prg)))]
    (apply-op (op (first (drop pos prg))) state)))

(defn seq!!
  "Returns a (blocking!) lazy sequence read from a channel."
  [c]
  (lazy-seq
   (when-let [v (a/<!! c)]
     (cons v (seq!! c)))))

(defn run-prg [prg input-ch]
  (let [output-ch (a/chan 10)
        output-fn (fn [v] (a/>!! output-ch v))
        input-fn  (fn [] (a/<!! input-ch))
        stop-fn   (fn [] (a/close! output-ch))
        steps     (iterate step {:prg       prg
                                 :input-fn  input-fn
                                 :output-fn output-fn
                                 :stop-fn   stop-fn
                                 :pos       0})]
    (future
      (doall (take-while #(not (:halt? %)) steps)))
    output-ch))

(defn run-prg!! [prg inputs]
  (let [out-ch (run-prg prg (a/to-chan inputs))]
    (seq!! out-ch)))
