(ns advent-of-code.day11
  (:require [advent-of-code.utils :as u]))

(def ^:private hex-steps
  {"n"  [0 -1 1],
   "ne" [1 -1 0],
   "se" [1 0 -1],
   "s"  [0 1 -1],
   "sw" [-1 1 0],
   "nw" [-1 0 1]})

(defn- find-endpoint
  "Find the ending point on the hex grid after all `steps` are taken"
  [start steps]
  (reduce (fn [cur-point dir]
            (mapv + cur-point (hex-steps dir))) start steps))

(defn- find-distance
  "Find the distance from the given start-point to the given end-point"
  [start end]
  (/ (apply + (map #(Math/abs %) (mapv - end start))) 2))

(defn part-1
  "Day 11 Part 1"
  [input]
  (->> input
       u/alphanum-tokenize
       (find-endpoint [0 0 0])
       (find-distance [0 0 0])))

(defn- find-far-distance
  "Find the farthest distance the child got on their path"
  [steps]
  (reduce (fn [{:keys [maxdist point]} dir]
            (let [point' (mapv + point (hex-steps dir))
                  dist   (/ (apply + (map #(Math/abs %) point')) 2)]
              {:maxdist (max maxdist dist), :point point'}))
          {:maxdist 0, :point [0 0 0]} steps))

(defn part-2
  "Day 11 Part 2"
  [input]
  (->> input
       u/alphanum-tokenize
       find-far-distance
       :maxdist))
