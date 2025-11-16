(ns advent-of-code.day02
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as comb]))

(defn- checksum
  "Calculate the checksum for the rows passed in"
  [rows]
  (let [diff-val (fn [row] (- (apply max row) (apply min row)))]
    (reduce + (map diff-val rows))))

(defn part-1
  "Day 02 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       checksum))

(defn- get-row-value
  "Get the row-level checksum value for the given row"
  [row]
  (let [row'  (sort row)
        pairs (comb/combinations row' 2)
        value (fn [[a b]] (if (zero? (rem b a)) (/ b a) 0))]
    (first (filter pos? (map value pairs)))))

(defn- checksum2
  "Calculate the second version of a checksum for the rows"
  [rows]
  (apply + (map get-row-value rows)))

(defn part-2
  "Day 02 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       checksum2))
