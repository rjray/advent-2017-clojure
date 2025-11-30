(ns advent-of-code.day19
  (:require [advent-of-code.utils :as u]))

(def ^:private dirs {:u [-1 0],
                     :r [0 1],
                     :d [1 0],
                     :l [0 -1]})
(def ^:private turns {:u [:l :r],
                      :d [:r :l],
                      :l [:d :u],
                      :r [:u :d]})

(defn- get-next-pos-and-dir
  "Get the next position and direction of the packet in `field`"
  [pos dir field]
  (let [next-by-dir  (mapv + pos (dirs dir))
        l-by-dir     (get-in turns [dir 0])
        left-by-dir  (mapv + pos (dirs l-by-dir))
        r-by-dir     (get-in turns [dir 1])
        right-by-dir (mapv + pos (dirs ((turns dir) 1)))]
    (first (filter #(get-in field (first %))
                   (list [next-by-dir dir]
                         [left-by-dir l-by-dir]
                         [right-by-dir r-by-dir])))))

(defn- get-letter-at
  "Return the character at `pos` if it is a letter"
  [pos field]
  (when pos
    (let [letter (get-in field pos)]
      (if (<= 65 (int letter) 90) letter nil))))

(defn- run-packet
  "Find the start-point and run the packet through the route"
  [field & [count-steps]]
  (let [[start-y start-x] [0 (.indexOf (first field) \|)]]
    (loop [pos [start-y start-x], dir :d, letters [], steps 1]
      (let [[next-pos next-dir] (get-next-pos-and-dir pos dir field)
            letter              (get-letter-at next-pos field)]
        (if (nil? next-pos)
          (if count-steps steps (apply str letters))
          (recur next-pos next-dir
                 (if letter (conj letters letter) letters) (inc steps)))))))

(defn part-1
  "Day 19 Part 1"
  [input]
  (-> input
      (u/to-matrix {\  nil})
      run-packet))

(defn part-2
  "Day 19 Part 2"
  [input]
  (-> input
      (u/to-matrix {\  nil})
      (run-packet true)))
