(ns advent-of-code.day05
  (:require [advent-of-code.utils :as u]))

(defn- find-steps-out
  "Find the number of steps to get out of the maze of jumps"
  [update-fn prog]
  (let [size (count prog)]
    (loop [prog prog, pc 0, step 0]
      (cond
        (>= pc size) step
        :else        (recur (update prog pc update-fn)
                            (+ pc (get prog pc))
                            (inc step))))))

(defn part-1
  "Day 05 Part 1"
  [input]
  (->> input
       u/to-lines
       (mapv parse-long)
       (find-steps-out inc)))

(defn part-2
  "Day 05 Part 2"
  [input]
  (->> input
       u/to-lines
       (mapv parse-long)
       (find-steps-out (fn [x] (if (> x 2) (dec x) (inc x))))))
