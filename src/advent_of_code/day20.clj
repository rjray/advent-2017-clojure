(ns advent-of-code.day20
  (:require [advent-of-code.utils :as u]))

(defn- m3-dist
  "Simpler 3-axis Manhattan distance since p2 is always (0, 0, 0)"
  [point]
  (apply + (map abs point)))

(defn- advance
  "Advance a single particle after adjusting velocity by acceleration"
  [[pos vel acc]]
  (let [vel' (mapv + vel acc)
        pos' (mapv + pos vel')]
    (vector pos' vel' acc)))

(defn- step
  "Step the `particles` simulation one iteration, updating `history` as well"
  [particles history]
  (let [particles' (mapv advance particles)
        dist-map   (into {} (map #(vector %1 %2)
                                 (range)
                                 (map #(m3-dist (first %)) particles')))
        sorted     (sort #(compare (last %1) (last %2)) dist-map)]
    (list particles' (cons (ffirst sorted) history))))

(defn- find-inner
  "Find the particle that stays closest to (0, 0, 0) over time"
  [particles]
  (loop [particles particles, history ()]
    (cond
      (and (< 500 (count history))
           (apply = (take 500 history))) (first history)
      :else
      (let [[particles' history'] (step particles history)]
        (recur particles' history')))))

(defn part-1
  "Day 20 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       (map #(partitionv 3 %))
       (mapv vec)
       find-inner))

(defn- sim-calc
  "Calculate the position of `particle` at time `t`"
  [particle t]
  (let [[pos vel acc] particle]
    (mapv +
          (mapv #(/ (* % t t) 2) acc)
          (mapv #(* % t) (mapv #(+ %1 (/ %2 2)) vel acc))
          pos)))

(defn- remove-collisions
  "Calculate all particle positions at `t` and remove any that collide"
  [particles t]
  (let [positions (into {} (map #(vector % (sim-calc % t)) particles))
        count-map (group-by val positions)]
    (vec (map ffirst (filter #(= 1 (count %)) (vals count-map))))))

(defn- find-survivors
  "Run the simulation, removing collisions, until there are no more collisions"
  [particles]
  (loop [particles particles, t 1, counts (list (count particles))]
    (if (and (< 100 (count counts)) (apply = (take 100 counts)))
      (first counts)
      (let [particles' (remove-collisions particles t)]
        (recur particles' (inc t) (cons (count particles') counts))))))

(defn part-2
  "Day 20 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       (map #(partitionv 3 %))
       (mapv vec)
       find-survivors))
