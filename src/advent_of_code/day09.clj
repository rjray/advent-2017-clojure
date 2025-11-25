(ns advent-of-code.day09
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn- score
  "Calculate the score for the analyzed stream"
  [counts]
  (reduce (fn [sum [val cnt]] (+ sum (* val cnt))) 0 counts))

(defn- score-groups
  "Parse the stream and produce a total score for all groups"
  [stream]
  (loop [[ch & st] stream, counts {}, depth 0, skip false, garbage false, gc 0]
    (if (nil? ch)
      (if (zero? depth)
        (list (score counts) gc)
        "Parsing mismatch, depth != 0.")
      ;; This is more or less a state-machine in which the state is tracked in
      ;; the `depth`, `skip` and `garbage` values.
      (cond
        ;; Skip the current character (last character was \!)
        skip      (recur st counts depth false garbage gc)
        ;; Mark the next character to be skipped
        (= ch \!) (recur st counts depth true garbage gc)
        ;; Marks the end of garbage whenever encountered
        (= ch \>) (recur st counts depth skip false gc)
        ;; If currently in a garbage group, skip this character
        garbage   (recur st counts depth skip garbage (inc gc))
        ;; Marks the start of garbage whenever encountered (doesn't nest)
        (= ch \<) (recur st counts depth skip true gc)
        ;; Marks the end of a group. Count the group and decrement `depth`.
        (= ch \}) (recur st (update counts depth (fn [v] (if v (inc v) 1)))
                         (dec depth) skip garbage gc)
        ;; Marks the start of a group. Increment `depth`.
        (= ch \{) (recur st counts (inc depth) skip garbage gc)
        ;; An ordinary character in a non-garbage group.
        :else     (recur st counts depth skip garbage gc)))))

(defn part-1
  "Day 09 Part 1"
  [input]
  (->> input
       str/trim
       score-groups
       first))

(defn part-2
  "Day 09 Part 2"
  [input]
  (->> input
       str/trim
       score-groups
       last))
