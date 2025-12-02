(ns advent-of-code.day24
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

(defn- strength
  "Find the strength of the given bridge according to the puzzle formula"
  [bridge]
  (apply + (flatten (seq bridge))))

(defn- find-matching
  "Return all available components with an end that matches `port`"
  [port avail]
  (filter #(<= 0 (.indexOf % port)) avail))

(defn- new-node
  "Create a new node by adding `part` to the existing `pth`"
  [pth nxt part]
  (let [[a b] part, new-end (if (= a nxt) b a)]
    {:pth (conj pth part), :nxt new-end}))

(defn- find-bridges
  "Find the max-strength bridge possible from the set of components"
  [components]
  (let [initial {:pth #{}, :nxt 0}
        queue   (into clojure.lang.PersistentQueue/EMPTY [initial])]
    (loop [queue queue, bridges ()]
      (let [node (peek queue), queue (pop queue)]
        (if (nil? node)
          bridges
          (let [{:keys [pth nxt]} node
                avail             (set/difference components pth)
                matches           (find-matching nxt avail)]
            (recur (into queue (map #(new-node pth nxt %) matches))
                   (cons [(count pth) (strength pth)] bridges))))))))

(defn part-1
  "Day 24 Part 1"
  [input]
  (->> (u/to-lines input)
       (map u/parse-out-longs)
       (map vec)
       set
       find-bridges
       (map last)
       (apply max)))

(defn- sort-bridges
  "Sort the list of potential bridges by strongest and longest"
  [bridges]
  (sort #(compare (first %2) (first %1))
        (sort #(compare (last %2) (last %1)) bridges)))

(defn part-2
  "Day 24 Part 2"
  [input]
  (->> (u/to-lines input)
       (map u/parse-out-longs)
       (map vec)
       set
       find-bridges
       sort-bridges
       first
       last))
