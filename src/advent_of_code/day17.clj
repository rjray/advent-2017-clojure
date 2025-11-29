(ns advent-of-code.day17
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- number-after
  "Given the memory state `mem` and the final position, find the next number"
  [mem pos]
  (let [size (count mem)
        pos' (mod (inc pos) size)
        mem' (set/map-invert mem)]
    (mem' pos')))

(defn- bump
  "Increment the value of the key `k` in `mem` if it is >= `val`"
  [mem k val]
  (update mem k (fn [v] (if (>= v val) (inc v) v))))

(defn- insert
  "Insert `n` into `mem` at a new position based on `pos` and `cycle`"
  [mem n pos cycle]
  (let [size (count mem)
        pos' (inc (mod (+ pos cycle) size))]
    (list (assoc (reduce (fn [memory k]
                           (bump memory k pos'))
                         mem (keys mem)) n pos')
          pos')))

(defn- find-number-after
  "Run the algorithm and return the number that ends up right after `n`"
  [n cycle]
  (loop [[n' & ns] (range 1 (inc n)), [mem pos] (list (sorted-map 0 0) 0)]
    (if (nil? n')
      (number-after mem pos)
      (recur ns (insert mem n' pos cycle)))))

(defn part-1
  "Day 17 Part 1"
  [input]
  (->> input
       str/trim
       parse-long
       (find-number-after 2017)))

(defn- next-pos
  "Calculate the next position to place a number"
  [size pos cycle]
  (inc (mod (+ pos cycle) size)))

(defn- find-number-after-0
  "Find the number after `0` in the sequence after `n` steps"
  [n cycle]
  (loop [[n & ns] (range 1 (inc n)), pos 0, num 0]
    (if (nil? n)
      num
      (let [pos' (next-pos n pos cycle)]
        (if (= 1 pos')
          (recur ns pos' n)
          (recur ns pos' num))))))

(defn part-2
  "Day 17 Part 2"
  [input]
  (->> input
       str/trim
       parse-long
       (find-number-after-0 50000000)))
