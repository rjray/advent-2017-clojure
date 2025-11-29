(ns advent-of-code.day18
  (:require [advent-of-code.utils :as u]))

(def ^:private ops
  {:snd (fn [prog [reg]]
          (assoc (update prog :pc inc)
                 :playing (get-in prog [:register reg])))
   :set (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (assoc-in (update prog :pc inc) [:register reg] val)))
   :add (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (update-in (update prog :pc inc) [:register reg] + val)))
   :mul (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (update-in (update prog :pc inc) [:register reg] * val)))
   :mod (fn [prog [reg arg]]
          (let [val (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (update-in (update prog :pc inc) [:register reg] mod val)))
   :rcv (fn [prog [reg]]
          (if (< 0 (get-in prog [:register reg]))
            (update (update prog :pc inc) :output conj (get prog :playing))
            (update prog :pc inc)))
   :jgz (fn [prog [reg arg]]
          (let [reg' (if (keyword? reg) (get-in prog [:register reg]) reg)
                arg' (if (keyword? arg) (get-in prog [:register arg]) arg)]
            (if (< 0 reg')
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
  (reduce (fn [prog line]
            (compile-line prog line))
          {:code [], :pc 0 :register {}, :output [], :playing nil} lines))

(defn- step
  "Process a single instruction"
  [prog [op & args]]
  ((ops op) prog args))

(defn- run-code
  "Run the program's code until there is a value in :output"
  [prog]
  (loop [prog prog]
    (let [{:keys [code pc output]} prog]
      (if (< 0 (count output))
        (first output)
        (recur (step prog (code pc)))))))

(defn part-1
  "Day 18 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/tokenize)
       assemble
       run-code))

;; Replace the :snd and :rcv ops, with an extra arg for the queues
(def ^:private vecr (comp vec rest))
(def ^:private ops'
  (assoc ops
         :snd (fn [prog [reg] queues]
                (let [val (get-in prog [:register reg])
                      pid (:pid prog)]
                  (list (update (update prog :pc inc) :sent inc)
                        (update-in queues [pid] conj val))))
         :rcv (fn [prog [reg] queues]
                (let [opid   (- 1 (:pid prog))
                      avail? (< 0 (count (queues opid)))]
                  (if avail?
                    (let [value   (first (queues opid))
                          queues' (update-in queues [opid] vecr)]
                      (list (assoc-in (update prog :pc inc)
                                      [:register reg] value)
                            queues'))
                    (list (assoc prog :waiting true) queues))))))

(defn- setup-duet-code
  "Create two copies of `prog`, each with register `p` set to the process ID"
  [prog]
  ;; Set up two programs, with reg :p set, a :pid, and a :waiting flag
  (mapv #(assoc (assoc-in prog [:register :p] %)
                :waiting false, :done false, :sent 0, :pid %) [0 1]))

(defn- step-prog
  "Process a single instruction for the given program"
  [prog queues]
  (if (:done prog)
    (list prog queues)
    (let [[op & args] (get (:code prog) (:pc prog) nil)]
      (case op
        :snd ((ops' :snd) prog args queues)
        :rcv ((ops' :rcv) prog args queues)
        ;; Remaining ops only return the updated `prog`, so add `queues`
        (list ((ops' op) prog args) queues)))))

(defn- run-duet-code
  "Run the two program in parallel"
  [[prog0 prog1]]
  (loop [prog0 prog0, prog1 prog1, queues [[], []]]
    (cond
      (and (or (:waiting prog0)
               (:done prog0))
           (or (:waiting prog1)
               (:done prog0)))  (:sent prog1)
      :else
      ;; Step each program once, tracking changes to `queues`
      (let [[prog0' queues'] (step-prog prog0 queues)
            [prog1' queues'] (step-prog prog1 queues')]
        (recur prog0' prog1' queues')))))

(defn part-2
  "Day 18 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/tokenize)
       assemble
       setup-duet-code
       run-duet-code))
