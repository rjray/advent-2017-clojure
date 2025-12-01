(ns advent-of-code.day22
  (:require [advent-of-code.utils :as u]))

(def ^:private dirs {:u [-1 0],
                     :r [0 1],
                     :d [1 0],
                     :l [0 -1]})
(def ^:private turns {:u [:l :r :u :d],
                      :d [:r :l :d :u],
                      :l [:d :u :l :r],
                      :r [:u :d :r :l]})

(defn- to-grid
  "Take a 'matrix' of values and create a grid-map from it. Matrix locations
  become the keys, and the value in the matrix at that point the value. The
  grid will be centered at [0 0], with coordinates adjusted."
  [matrix]
  (let [Y   (count matrix)
        dY  (int (/ Y 2))
        X   (count (first matrix))
        dX  (int (/ X 2))]
    (into {} (for [y (range Y), x (range X)
                   :let [y' (- y dY), x' (- x dX), ch (get-in matrix [y x])]]
               [[y' x'] ch]))))

(defn- activate
  "Activate the virus `n` times on the given `grid`. Return number of grid
  points that were actively infected by the virus."
  [n grid]
  (loop [grid grid, pos [0 0], dir :u, n n, infections 0]
    (if (zero? n)
      infections
      (let [current (get grid pos 0)
            new-dir (get-in turns [dir current])
            new-val (- 1 current)
            new-pos (mapv + pos (dirs new-dir))]
        (recur (assoc grid pos new-val)
               new-pos
               new-dir
               (dec n)
               (+ infections new-val))))))

(defn part-1
  "Day 22 Part 1"
  [input]
  (->> (u/to-matrix input {\. 0, \# 1})
       to-grid
       (activate 10000)))

(defn- activate2
  "Activate the virus `n` times on the given `grid`. Return number of grid
  points that were actively infected by the virus. Based on the new 'rules'."
  [n grid]
  (let [nextval [2 3 1 0]]
    (loop [grid grid, pos [0 0], dir :u, n n, infections 0]
      (if (zero? n)
        infections
        (let [current (get grid pos 0)
              new-dir (get-in turns [dir current])
              new-val (nextval current)
              new-pos (mapv + pos (dirs new-dir))]
          (recur (assoc grid pos new-val)
                 new-pos
                 new-dir
                 (dec n)
                 (if (= new-val 1) (inc infections) infections)))))))

(defn part-2
  "Day 22 Part 2"
  [input]
  (->> (u/to-matrix input {\. 0, \# 1})
       to-grid
       (activate2 10000000)))
