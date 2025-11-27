(ns advent-of-code.day15
  (:require [advent-of-code.utils :as u]))

(defn- next-val
  "Generate the next value for the 'Dueling Generators', based on their values"
  [key current]
  (rem (* current key) 2147483647))

(defn- generator
  "Create a 'generator' that is implemented as a lazy-sequence"
  [key start]
  (cons start (lazy-seq (generator key (next-val key start)))))

(defn- create-generators
  "Create the two generators"
  [gen-A-key gen-B-key [gen-A-start gen-B-start]]
  (list (generator gen-A-key gen-A-start)
        (generator gen-B-key gen-B-start)))

(defn- check?
  "Determine if the two values match on the low 16 bits"
  [[a b]]
  (= (bit-and a 65535) (bit-and b 65535)))

(defn- judge
  "Pull `n` samples from each generator and count the ones that pass `check`"
  [n [gen-a gen-b]]
  (loop [[pair & pairs] (map list gen-a gen-b), n n, count 0]
    (if (zero? n)
      count
      (if (check? pair)
        (recur pairs (dec n) (inc count))
        (recur pairs (dec n) count)))))

(defn part-1
  "Day 15 Part 1"
  [input]
  (->> input
       u/parse-out-longs
       (create-generators 16807 48271)
       (judge 40000000)))

(defn- to-filters
  "Convert the generator lazy-seqs to filter lazy-seqs"
  [[gen-a gen-b]]
  (list (filter #(zero? (mod % 4)) gen-a)
        (filter #(zero? (mod % 8)) gen-b)))

(defn part-2
  "Day 15 Part 2"
  [input]
  (->> input
       u/parse-out-longs
       (create-generators 16807 48271)
       to-filters
       (judge 40000000)))
