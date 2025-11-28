(ns advent-of-code.day16
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- spin
  "Spin `n` dancers from the end of the vector to the beginning, keeping order"
  [dancers n]
  (vec (concat (take-last n dancers) (drop-last n dancers))))

(defn- exchange
  "Exchange the dancers at positions `a` and `b`"
  [dancers [a b]]
  (assoc (assoc dancers a (dancers b)) b (dancers a)))

(defn- partner
  "Swap the positions of dancers named `a` and `b`"
  [dancers [a b]]
  (exchange dancers [(.indexOf dancers a) (.indexOf dancers b)]))

(defn- dance
  "Make them dance"
  [steps dancers]
  (loop [[step & steps] steps, dancers dancers]
    (if (nil? step)
      dancers
      (let [[move detail] (str/split step #"" 2)]
        (case move
          "s" (recur steps (spin dancers (parse-long detail)))
          "x" (recur steps (exchange dancers (map parse-long
                                                  (str/split detail #"/"))))
          "p" (recur steps (partner dancers (map #(first (char-array %))
                                                 (str/split detail #"/")))))))))

(defn part-1
  "Day 16 Part 1"
  [input & [dancers]]
  (let [dancers (or dancers "abcdefghijklmnop")]
    (-> input
        str/trim
        (str/split #",")
        (dance (vec dancers))
        ((fn [x] (apply str x))))))

(defn- find-by-pos
  "Find by position the arrangement that would be the billionth one"
  [dancers seen iter]
  (let [first-pos (seen dancers)
        diff      (- iter first-pos)
        offset    (mod (- 1000000000 first-pos) diff)
        seen'     (set/map-invert seen)]
    (seen' offset)))

(defn- dance-billion
  "Simulate running the dance 1,000,000,000 times over"
  [steps dancers]
  (loop [dancers dancers, seen {}, iter 0]
    (cond
      (= iter 1000000000) dancers
      (seen dancers)      (find-by-pos dancers seen iter)
      :else               (recur (dance steps dancers)
                                 (assoc seen dancers iter)
                                 (inc iter)))))

(defn part-2
  "Day 16 Part 2"
  [input]
  (-> input
      str/trim
      (str/split #",")
      (dance-billion (vec "abcdefghijklmnop"))
      ((fn [x] (apply str x)))))
