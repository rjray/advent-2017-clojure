(ns advent-of-code.day06
  (:require [advent-of-code.utils :as u]))

(defn- distribute
  "Find the largest memory bank and distribute the content"
  [mem size]
  (let [[idx cnt] (first (sort #(compare (last %2) (last %1))
                               (map #(list %1 %2) (iterate inc 0) mem)))
        all       (quot cnt size)
        xtra      (rem cnt size)]
    (reduce (fn [mem' idx']
              (update mem' (mod (+ 1 idx' idx) size) inc))
            (mapv #(+ all %) (assoc mem idx 0))
            (range xtra))))

(defn- find-loop-count
  "Find the number of steps needed to hit the loop"
  [meml]
  (let [mem  (vec meml)
        size (count mem)]
    (loop [mem mem, step 1, seen #{mem}]
      (let [mem' (distribute mem size)]
        (if (seen mem') step (recur mem' (inc step) (conj seen mem')))))))

(defn part-1
  "Day 06 Part 1"
  [input]
  (->> input
       u/parse-out-longs
       find-loop-count))

(defn- find-loop-count-2
  "Find the number of steps needed to hit the loop"
  [meml]
  (let [mem  (vec meml)
        size (count mem)]
    (loop [mem mem, step 1, seen #{mem}, places {mem 0}]
      (let [mem' (distribute mem size)]
        (if (seen mem')
          (- step (places mem'))
          (recur mem' (inc step) (conj seen mem') (assoc places mem' step)))))))

(defn part-2
  "Day 06 Part 2"
  [input]
  (->> input
       u/parse-out-longs
       find-loop-count-2))
