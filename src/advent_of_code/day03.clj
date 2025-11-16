(ns advent-of-code.day03
  (:require [advent-of-code.utils :as u]))

;; Algorithm from https://math.stackexchange.com/a/163101
(defn- spiral
  "Find the coordinates of the n-th spiral number"
  [n]
  (let [k (long (Math/ceil (/ (- (Math/sqrt n) 1.0) 2.0)))
        t (long (inc (* 2 k)))
        m (* t t)
        t (dec t)]
    (if (>= n (- m t))
      [(- k (- m n)) (- k)]
      (let [m (- m t)]
        (if (>= n (- m t))
          [(- k) (+ (- k) (- m n))]
          (let [m (- m t)]
            (if (>= n (- m t))
              [(+ (- k) (- m n)) k]
              [k (- k (- m n t))])))))))

(defn part-1
  "Day 03 Part 1"
  [input]
  (->> input
       u/parse-out-longs
       first
       spiral
       (u/manhattan [0 0])))

(def ^:private neighbor-pairs (list [-1 1]  [0 1]  [1 1]
                                    [-1 0]         [1 0]
                                    [-1 -1] [0 -1] [1 -1]))

(defn- neighbors
  "Get the eight neighbor coordinate pairs around `point`"
  [point]
  (map #(mapv + point %) neighbor-pairs))

(defn- neighbor-values-sum
  "Get the values from any currently-set neighbors of `point` in `field`"
  [point field]
  (apply + (map #(get field % 0) (neighbors point))))

(defn- find-value
  "Find the first value written to memory that is higher than `thresh`"
  [thresh]
  (loop [field {[0 0] 1}, n 2]
    (let [point (spiral n)
          value (neighbor-values-sum point field)]
      (if (> value thresh)
        value
        (recur (assoc field point value) (inc n))))))

(defn part-2
  "Day 03 Part 2"
  [input]
  (->> input
       u/parse-out-longs
       first
       find-value))
