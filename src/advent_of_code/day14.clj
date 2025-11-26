(ns advent-of-code.day14
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

;; Map the characters that can come from a hex string into binary strings
(def ^:private binmap
  {\0 "0000",
   \1 "0001",
   \2 "0010",
   \3 "0011",
   \4 "0100",
   \5 "0101",
   \6 "0110",
   \7 "0111",
   \8 "1000",
   \9 "1001",
   \a "1010",
   \b "1011",
   \c "1100",
   \d "1101",
   \e "1110",
   \f "1111"})

;; These fns (hash-rope, create-lengths, hash-n-times, make-dense-hash,
;; to-hex-string, gen-hash) are copied from day10.clj because I don't feel
;; like making them into a separate library right now.

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

;; This is slightly modified to hard-code the number of hash iterations at 64
(defn- gen-hash
  "Generate the 32-character hash string for the data"
  [lengths]
  (as-> (vec (range 256)) $
    (hash-n-times 64 $ lengths)
    (make-dense-hash $)
    (to-hex-string $)))

(defn- binary-rep
  "Convert a hexadecimal string into a binary representation"
  [hexstr]
  (apply str (map binmap hexstr)))

(defn- get-row-count
  "Get the count of 'used blocks' for one row, based on the hash-key given"
  [hash-key]
  (count (filter #(= % \1) (binary-rep (gen-hash (create-lengths hash-key))))))

(defn- get-used-block-count
  "Get the count of used blocks on the 128x128 disk grid"
  [keybase]
  (reduce + (map #(get-row-count (str keybase "-" %)) (range 128))))

(defn part-1
  "Day 14 Part 1"
  [input]
  (->> input
       str/trim
       get-used-block-count))

(defn- rowvec
  "Create the vector of 0/1 values for a given hash key"
  [base n]
  (let [hashkey (str base "-" n)]
    (vec (binary-rep (gen-hash (create-lengths hashkey))))))

(defn- make-field
  "Make the disk field from the base hash key value. Returns vec of vecs."
  [keybase]
  (mapv #(rowvec keybase %) (range 128)))

(def ^:private dirs (list [-1 0] [1 0] [0 -1] [0 1]))

(defn- find-region
  "Find a single region in the pool of used blocks. Return a set of the coords."
  [point used]
  (let [queue (into clojure.lang.PersistentQueue/EMPTY [point])]
    (loop [queue queue, seen #{}, region ()]
      (let [pt (peek queue), queue (pop queue)]
        (cond
          (nil? pt) region
          (seen pt) (recur queue seen region)
          :else
          (let [adj (filter used (map #(mapv + pt %) dirs))]
            (recur (into queue adj) (conj seen pt) (cons pt region))))))))

(defn- find-regions
  "Find the regions in the disk-map given as `field`"
  [field]
  (let [used (set (for [y (range 128), x (range 128)
                        :when (= \1 (get-in field [y x]))]
                    [y x]))]
    (loop [used used, regions 0]
      (if (empty? used)
        regions
        (let [point  (first used)
              region (find-region point used)]
          (recur (set/difference used region) (inc regions)))))))

(defn part-2
  "Day 14 Part 2"
  [input]
  (->> input
       str/trim
       make-field
       find-regions))
