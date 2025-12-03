(ns advent-of-code.day25
  (:require [advent-of-code.utils :as u]))

(defn- init-machine
  "Initialize the faux-Turing machine"
  [inst]
  (let [[_ state steps] (re-find #"(?s)state ([A-Z])[.].*after (\d+)" inst)
        steps           (parse-long steps)]
    (list steps {:tape {0 0}, :states {}, :pos 0, :current (keyword state)})))

;; Extended regexp pattern to extract data from a state description
(def ^:private pat-base "value ([01]).*?to the (right|left).*?state ([A-Z])")
(def ^:private pattern
  (re-pattern (str "(?s)In state ([A-Z]).*?" pat-base ".*?" pat-base)))
(def ^:private writes {"0" 0, "1" 1})
(def ^:private moves {"left" -1, "right" 1})

(defn- fixup
  "Clean up the three values parsed from a state-block"
  [[a b c]]
  [(writes a) (moves b) (keyword c)])

(defn- apply-state-desc
  "Take and parse a single state description. Add it to `machine`."
  [machine desc]
  (let [[_ state & data] (re-find pattern desc)
        state            (keyword state)
        data             (mapv fixup (partitionv 3 data))]
    (assoc-in machine [:states state] data)))

(defn- create-config
  "Turn the text description into a state machine"
  [spec]
  (let [[init & states] (u/to-blocks spec)
        [steps mach]    (init-machine init)]
    (list steps (reduce apply-state-desc mach states))))

(defn- step
  "Advance the machine one time"
  [m]
  (let [{:keys
         [tape pos current]}    m
        [write move next-state] (get-in m [:states current (get tape pos 0)])]
    (assoc m
           :pos (+ pos move)
           :current next-state
           :tape (assoc tape pos write))))

(defn- run-config
  "Use the given configuration to run `machine` `n` steps and checksum"
  [[n machine]]
  (loop [machine machine, iter 0]
    (if (= iter n)
      (count (get (group-by val (:tape machine)) 1))
      (recur (step machine) (inc iter)))))

(defn part-1
  "Day 25 Part 1"
  [input]
  (->> (create-config input)
       run-config))

(defn part-2
  "Day 25 Part 2"
  [input]
  "Congrats! You should have all 50 stars by now!")
