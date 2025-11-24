(ns advent-of-code.day07
  (:require [advent-of-code.utils :as u]
            [clojure.set :as set]))

(defn- setup-struct
  "Take the lines of tokens and set up what is needed for part 2"
  [lines]
  (loop [[line & lines] lines, structure {:weight {}, :children {}}]
    (if (nil? line)
      structure
      (let [[name weight & children] line
            name                     (keyword name)
            weight                   (parse-long weight)
            children                 (map keyword children)]
        (recur lines (-> structure
                         (assoc-in [:weight name] weight)
                         (assoc-in [:children name] children)))))))

(defn- get-root
  "Get the label of the root of the tree"
  [data]
  (let [all-nodes (set (keys data))
        children  (set (flatten (vals data)))]
    (first (set/difference all-nodes children))))

(defn part-1
  "Day 07 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/alphanum-tokenize)
       setup-struct
       :children
       get-root))

(defn- solve-2
  "Solve part 2 of the problem with a recursive depth-first search."
  [data]
  (letfn [(total-weight [node atm]
            (let [weight     (get-in data [:weight node])
                  children   (get-in data [:children node])
                  subweights (map #(total-weight % atm) children)]
              (if (> (count (set subweights)) 1)
                (let [table     (into {} (map hash-map children subweights))
                      groups    (vals (group-by last table))
                      single    (ffirst (filter #(= 1 (count %)) groups))
                      multi     (ffirst (filter #(not (= 1 (count %))) groups))
                      target    (last multi)
                      failure   (last single)
                      corrected (+ (- target failure)
                                   (get-in data [:weight (first single)]))
                      _         (swap! atm conj corrected)]
                  (+ weight (apply + subweights)))
                (+ weight (apply + subweights)))))]
    (let [root   (get-root (:children data))
          values (atom ())
          _      (total-weight root values)]
      (last @values))))

(defn part-2
  "Day 07 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/alphanum-tokenize)
       setup-struct
       solve-2))
