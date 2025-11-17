(ns advent-of-code.day04
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

(defn- is-valid?
  "Predicate to determine if the given passphrase is valid"
  [phrase]
  (loop [[word & words] phrase, seen #{}]
    (cond
      (nil? word) true
      (seen word) false
      :else       (recur words (conj seen word)))))

(defn part-1
  "Day 04 Part 1"
  [input]
  (->> input
       u/to-lines
       (map #(str/split % #" "))
       (filter is-valid?)
       count))

(defn- no-anagrams?
  "Predicate to determine if any passphrase words are anagrams of each other"
  [phrase]
  (let [pairs   (comb/combinations (map #(group-by identity %) phrase) 2)
        anagram (fn [[a b]] (= a b))]
    (not-any? anagram pairs)))

(defn part-2
  "Day 04 Part 2"
  [input]
  (->> input
       u/to-lines
       (map #(str/split % #" "))
       (filter is-valid?)
       (filter no-anagrams?)
       count))
