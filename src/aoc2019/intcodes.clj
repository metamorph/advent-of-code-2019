(ns aoc2019.intcodes
  (:require [clojure.core.async :as a]))

(def ^:dynamic DEBUG? false)

(defn read-vals [n {:keys [prg pos]}]
  (map #(get prg % 0) (range pos (+ pos n))))

(defn op
  "Split an integer into op-code and parameter types: `n -> [code p1 p2 p3]`"
  [n]
  (let [code (rem n 100)
        p1 (quot (rem n 1000) 100)
        p2 (quot (rem n 10000) 1000)
        p3 (quot (rem n 100000) 10000)]
    (if DEBUG? (printf "op(%d -> %s)\n" n [code p1 p2 p2]))
    [code p1 p2 p3]))

(defn read-pos [{:keys [prg offset]} v mode]
  (case mode
    0 (get prg v 0)
    1 v
    2 (get prg (+ offset v) 0)))

(defn write-pos [{:keys [prg offset]} v mode]
  (case mode
    0 v
    1 v
    2 (+ offset v)))


(defmulti apply-op (fn [[code & _] _] code))

;; Addition
(defmethod apply-op 1 [[_ m1 m2 m3] {:keys [pos prg] :as state}]
  (let [[a b c :as in] (read-vals 3 state)
        a'             (read-pos state a m1)
        b'             (read-pos state b m2)
        c'             (write-pos state c m3)]
    (-> state
        (update :pos + 3)
        (update :prg assoc c' (+ a' b')))))

;; Multiply
(defmethod apply-op 2 [[_ m1 m2 m3] {:keys [pos prg] :as state}]
  (let [[a b c] (read-vals 3 state)
        a'      (read-pos state a m1)
        b'      (read-pos state b m2)
        c'      (write-pos state c m3)]
    (-> state
        (update :pos + 3)
        (update :prg assoc c' (* a' b')))))

;; Read input and store it
(defmethod apply-op 3 [[_ m1 _ _] {:keys [pos prg offset input-fn] :as state}]
  (let [a  (first (read-vals 1 state))
        a' (write-pos state a m1)
        in (input-fn)]
    (assert (not= m1 1))
    (-> state
        (update :pos + 1)
        (update :prg assoc a' in))))

;; Write output
(defmethod apply-op 4 [[_ m1 _ _] {:keys [pos prg output-fn] :as state}]
  (let [a (first (read-vals 1 state))
        v (read-pos state a m1)]
    (output-fn v)
    (update state :pos + 1)))

;; Jump if true
(defmethod apply-op 5 [[_ m1 m2 _] {:keys [pos prg] :as state}]
  (let [[a b] (read-vals 2 state)
        a'    (read-pos state a m1)
        b'    (read-pos state b m2)]
    (if (pos? a')
      (assoc state :pos b')
      (update state :pos + 2))))

;; Jump if false
(defmethod apply-op 6 [[_ m1 m2 _] {:keys [pos prg] :as state}]
  (let [[a b] (read-vals 2 state)]
    (if (zero? (read-pos state a m1))
      (assoc state :pos (read-pos state b m2))
      (update state :pos + 2))))

;; Less-than [a < b, store 1 in c, else 0]
(defmethod apply-op 7 [[_ m1 m2 m3] {:keys [pos prg] :as state}]
  (let [[a b c] (read-vals 3 state)
        a'      (read-pos state a m1)
        b'      (read-pos state b m2)
        c'      (write-pos state c m3)]
    (-> state
        (update :pos + 3)
        (update :prg assoc c' (if (< a' b') 1 0)))))

;; Equals [a == b, store 1 in c, else 0]
(defmethod apply-op 8 [[_ m1 m2 m3] {:keys [pos prg] :as state}]
  (let [[a b c] (read-vals 3 state)
        a'      (read-pos state a m1)
        b'      (read-pos state b m2)
        c'      (write-pos state c m3)]
    (-> state
        (update :pos + 3)
        (update :prg assoc c' (if (= a' b') 1 0)))))

;; End state
(defmethod apply-op 99 [_ {:keys [stop-fn] :as state}]
  (stop-fn)
  (assoc state :halt? true))

;; Handle relative-base-offset
(defmethod apply-op 9 [[_ m1 _ _] {:keys [pos prg offset] :as state}]
  (let [a (first (read-vals 1 state))]
    (-> state
        (update :pos inc)
        (update :offset + (read-pos state a m1)))))

(defn step [{:keys [pos prg offset] :as state}]
  (try
    (let [[code m1 m2 m3 :as pfx] (op (first (read-vals 1 state)))]
      (if DEBUG? (printf "Pos: %d, Offset: %d\n" pos offset))
      (apply-op pfx (update state :pos inc)))
    (catch Exception e
      (println "Error evaluating: " e)
      (throw (ex-info "Error evaluating" {:state state} e)))))

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
        steps     (iterate step {:prg       (zipmap (iterate inc 0) prg)
                                 :offset    0
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

(defn verify []
  (assert (= [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
             (run-prg!! [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] [])))
  (assert (= [1125899906842624] (run-prg!! [104,1125899906842624,99] [])))
  (assert (= [999] (run-prg!! [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [5])))
  (assert (= [1000] (run-prg!! [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [8])))
  (assert (= [1001] (run-prg!! [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [12])))
  "OK")
