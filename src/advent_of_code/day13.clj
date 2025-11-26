(ns advent-of-code.day13
  (:require [advent-of-code.utils :as u]))

(defn- create-schedule
  "Create the 'schedule' of scanners from the lines of data"
  [lines]
  (reduce (fn [sched [depth size]]
            (assoc sched depth [size (+ size (- size 2))])) {} lines))

(defn- get-severity
  "Determine the number of scanner-hits and calculate the score from them"
  [start schedule]
  (let [max-layer (apply max (keys schedule))]
    (reduce (fn [hits tick]
              (let [[size cycle] (get schedule tick nil)]
                (if (and cycle
                         (zero? (mod (+ tick start) cycle)))
                  (cons (* tick size) hits)
                  hits)))
            () (range (inc max-layer)))))

(defn part-1
  "Day 13 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       create-schedule
       (get-severity 0)
       (apply +)))

(defn- find-safe-entry
  "Find the number of ticks needed for the delay, to have safe passage"
  [schedule]
  (reduce (fn [acc tick]
            (if (empty? (get-severity acc schedule))
              (reduced acc)
              tick)) (range)))

(defn part-2
  "Day 13 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       create-schedule
       find-safe-entry))
