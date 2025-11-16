(ns advent-of-code.day01
  (:require [advent-of-code.utils :as u]))

(defn- calc-captcha
  "Calculate the captcha value from the input string"
  [input]
  (let [digits (u/->digits input)
        final  (list (first digits) (last digits))
        pairs  (partition 2 1 digits)
        value  (fn [[a b]] (if (= a b) a 0))]
    (+ (value final) (reduce + (map value pairs)))))

(defn part-1
  "Day 01 Part 1"
  [input]
  (->> input
       u/to-lines
       first
       calc-captcha))

(defn- calc-captcha-2
  "Calculatee the second captcha value from the input string"
  [input]
  (let [digits   (u/->digits input)
        offset   (/ (count digits) 2)
        rotation (concat (nthrest digits offset) (take offset digits))
        value    (fn [a b] (if (= a b) a 0))]
    (reduce + (map value digits rotation))))

(defn part-2
  "Day 01 Part 2"
  [input]
  (->> input
       u/to-lines
       first
       calc-captcha-2))
