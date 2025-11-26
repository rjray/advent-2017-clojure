(ns advent-of-code.day10
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn- hash-rope
  "Perform the 'hash' function on the given rope"
  [rope pos skip lengths]
  (let [size (count rope)
        wrap (fn [x] (mod x size))]
    (loop [[len & lengths] lengths, rope rope, skip skip, pos pos]
      (cond
        (nil? len)  (list rope pos skip)
        (zero? len) (recur lengths rope (inc skip) (wrap (+ pos skip)))
        :else
        (let [slice (mapv wrap (range pos (+ pos len)))
              nums  (replace rope slice)]
          (recur lengths (reduce (fn [rope' [pos num]]
                                   (assoc rope' pos num))
                                 rope (map list slice (reverse nums)))
                 (inc skip) (wrap (+ pos len skip))))))))

(defn part-1
  "Day 10 Part 1"
  [input & [size]]
  (let [size (or size 256)
        rope (vec (range size))]
    (->> input
         u/parse-out-longs
         (hash-rope 0 0 rope)
         first
         (take 2)
         (apply *))))

(defn- create-lengths
  "Take the string input and create a list of lengths"
  [input]
  (concat (map int input) (list 17 31 73 47 23)))

(defn- hash-n-times
  "Run the data through `hash-rope` `n` times"
  [n data lengths]
  (loop [data data, pos 0, skip 0, n n]
    (if (zero? n)
      data
      (let [[data' pos' skip'] (hash-rope data pos skip lengths)]
        (recur data' pos' skip' (dec n))))))

(defn- make-dense-hash
  "Take the vector of numbers from the hashing process and make 16 from them"
  [sparse]
  (map #(apply bit-xor %) (partition 16 sparse)))

(defn- to-hex-string
  "Take the sequence of numbers (bytes) and return a hex string"
  [bytes]
  (apply str (map #(format "%02x" %) bytes)))

(defn- gen-hash
  "Generate the 32-character hash string for the data"
  [times lengths]
  (as-> (vec (range 256)) $
    (hash-n-times times $ lengths)
    (make-dense-hash $)
    (to-hex-string $)))

(defn part-2
  "Day 10 Part 2"
  [input]
  (->> input
       str/trimr
       create-lengths
       (gen-hash 64)))
