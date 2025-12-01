(ns advent-of-code.day21
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.pprint]))

(def ^:private start-pattern
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])

(defn- create-rules
  "Create a table of rules from the input"
  [input]
  (let [pairs (reduce (fn [pairs line]
                        (conj pairs
                              (mapv #(u/create-field 0 0 (str/split % #"/"))
                                    (str/split line #" => "))))
                      [] (u/to-lines input))
        rules (into {} pairs)]
    (into {} pairs)))

(defn- extract-tile
  "Extract one NxN tile from `canvas`, with (Y, X) as the upper-left corner"
  [canvas N Y X]
  (vec (partitionv N (for [Y' (range N), X' (range N)
                           :let [y (+ Y Y'), x (+ X X')]]
                       (get-in canvas [y x])))))

(defn- split-canvas
  "Split the input canvas into sub-grids based on whether it's divisible by 2"
  [canvas split-by per-side]
  (for [y (range per-side), x (range per-side)]
    (extract-tile canvas split-by (* y split-by) (* x split-by))))

(defn- variants
  "Return all (distinct) variants of `tile`"
  [tile]
  (distinct (reduce (fn [variants tile]
                      (let [flip (vec (reverse tile))]
                        (cons tile (cons flip variants))))
                    () (take 4 (iterate u/rotate-cw tile)))))

(defn- expand-by-rule
  "Create a new tile from the given one, according to the patterns in `rules`"
  [rules tile]
  (some rules (variants tile)))

(defn- overlay
  "Overlay the given `tile` into `canvas` at the position [y x]"
  [canvas tile [y x]]
  (let [tile-width (count (first tile))
        y-off      (* y tile-width)
        x-off      (* x tile-width)
        pairs      (for [y' (range tile-width), x' (range tile-width)]
                     [[(+ y' y-off) (+ x' x-off)] (get-in tile [y' x'])])]
    (reduce (fn [canv [pos ch]]
              (assoc-in canv pos ch)) canvas pairs)))

(defn- stitch-canvas
  "Stitch together the collection of tiles into a new canvas"
  [per-side tiles]
  (let [tile-field (u/create-field 0 0 (partition per-side tiles))
        side-len   (* per-side (count (first tiles)))
        squares    (for [y (range per-side), x (range per-side)] [y x])]
    (reduce (fn [canv pos]
              (overlay canv (get-in tile-field pos) pos))
            (u/create-field side-len side-len) squares)))

(defn- expand-canvas
  "Expand the given canvas using the rules"
  [canvas rules]
  (let [side (count canvas)
        split-by (if (even? side) 2 3)
        per-side (/ side split-by)
        canvas'  (->> (split-canvas canvas split-by per-side)
                      (map #(expand-by-rule rules %))
                      (stitch-canvas per-side))]
    canvas'))

(defn- expand
  "Expand `start-pattern` for `n` iterations, according to the rules passed in"
  [n rules]
  (loop [canvas start-pattern, iter 0]
    (if (= iter n)
      canvas
      (recur (expand-canvas canvas rules) (inc iter)))))

(defn part-1
  "Day 21 Part 1"
  [input]
  (->> input
       create-rules
       (expand 5)
       flatten
       (filter #(= % \#))
       count))

(defn part-2
  "Day 21 Part 2"
  [input]
  (->> input
       create-rules
       (expand 18)
       flatten
       (filter #(= % \#))
       count))
