(ns advent-of-code.day23
  (:require [advent-of-code.utils :as u]))

(def ^:private ops
  {:set (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (assoc-in (update prog :pc inc) [:register reg] val)))
   :sub (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (update-in (update prog :pc inc) [:register reg] - val)))
   :mul (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (update-in (update prog :pc inc) [:register reg] * val)))
   :jnz (fn [prog [reg arg]]
          (let [reg' (if (keyword? reg) (get-in prog [:register reg]) reg)
                arg' (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (if (not (= 0 reg'))
              (update prog :pc + arg')
              (update prog :pc inc))))})

(defn- compile-line
  "Compile a single line, adding it to `prog`"
  [prog [op reg arg]]
  (let [op  (keyword op)
        reg (let [digits (re-find #"-?\d+" reg)]
              (if digits (parse-long digits) (keyword reg)))
        arg (when arg (let [digits (re-find #"-?\d+" arg)]
                        (if digits (parse-long digits) (keyword arg))))]
    (if (keyword? reg)
      (update (assoc-in prog [:register reg] 0)
              :code conj (if arg (list op reg arg) (list op reg)))
      (update prog :code conj (if arg (list op reg arg) (list op reg))))))

(defn- assemble
  "Turn the tokenized input into a program representation"
  [lines]
  (let [registers (into {} (map #(vector (keyword %) 0) (seq "abcdefgh")))]
    (reduce (fn [prog line]
              (compile-line prog line))
            {:code [], :pc 0 :register registers} lines)))

(defn- step
  "Process a single instruction"
  [prog [op & args]]
  ((ops op) prog args))

(defn- run-code
  "Run the program's code until it ends, counting the times `mul` is seen"
  [prog]
  (let [proglen (count (:code prog))]
    (loop [prog prog, mulcnt 0]
      (let [{:keys [code pc]} prog]
        (if (<= proglen pc)
          mulcnt
          (if (= (first (code pc)) :mul)
            (recur (step prog (code pc)) (inc mulcnt))
            (recur (step prog (code pc)) mulcnt)))))))

(defn part-1
  "Day 23 Part 1"
  [input]
  (->> (u/to-lines input)
       (map u/tokenize)
       assemble
       run-code))

(defn- run-code2
  "The 'machine code' is actually trying to find all non-prime numbers between
  two large-ish numbers. So we do that by getting all the numbers that ARE prime
  in that range and counting the ones that aren't in that set."
  [prog]
  (let [base  (last (first (:code prog)))
        b_val (+ 100000 (* 100 base))
        c_val (+ 17000 b_val)
        primes' (set (drop-while #(< % b_val)
                                 (take-while #(<= % c_val) u/primes)))]
    (count (filter #(not (primes' %)) (range b_val (inc c_val) 17)))))

(defn part-2
  "Day 23 Part 2"
  [input]
  (->> (u/to-lines input)
       (map u/tokenize)
       assemble
       run-code2))
