(ns advent-of-code.day12
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

(defn- build-graph
  "Build the graph representation of the data"
  [data]
  (reduce (fn [graph [id & links]] (assoc graph id (set links))) {} data))

(defn- neighborhood
  "Get the 'neighborhood' of the specified ID in `graph`"
  [id graph]
  (loop [queue     (into clojure.lang.PersistentQueue/EMPTY [id])
         neighbors #{id}
         seen      #{}]
    (let [id (peek queue), queue (pop queue)]
      (cond
        (nil? id) neighbors
        (seen id) (recur queue neighbors seen)
        :else     (let [new (graph id)]
                    (recur (into queue new)
                           (set/union neighbors new)
                           (conj seen id)))))))

(defn part-1
  "Day 12 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       build-graph
       (neighborhood 0)
       count))

(defn- get-neighborhoods
  "Get all 'neighborhoods represented in the graph data"
  [graph]
  (loop [graph graph, neighborhoods ()]
    (if (empty? graph)
      neighborhoods
      (let [new-hood (neighborhood (first (keys graph)) graph)]
        (recur (reduce dissoc graph new-hood) (cons new-hood neighborhoods))))))

(defn part-2
  "Day 12 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/parse-out-longs)
       build-graph
       get-neighborhoods
       count))
