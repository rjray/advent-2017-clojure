(ns advent-of-code.day08
  (:require [advent-of-code.utils :as u]))

(def ^:private op-table
  {"inc" +, "dec" -, "<"   <, ">"   >, "=="  =, "!="  not=, "<="  <=, ">="  >=})

(defn- compile-code
  "Do a 'compilation' of the code to something that can be easily processed"
  [lines]
  (loop [[line & lines] lines, repr {:code [], :register {}}]
    (if (nil? line)
      repr
      (let [[reg op val _ lhs cmp rhs] line]
        (recur lines (update-in (assoc-in repr [:register (keyword reg)] 0)
                                [:code] conj [(op-table op)
                                              (keyword reg)
                                              (parse-long val)
                                              (op-table cmp)
                                              (keyword lhs)
                                              (parse-long rhs)]))))))

(defn- run-code
  "'Run' the code given"
  [repr]
  (loop [[ins & code] (:code repr), repr repr]
    (if (nil? ins)
      repr
      (let [[op reg val cmp lhs rhs] ins]
        (if (cmp (get-in repr [:register lhs]) rhs)
          (recur code (update-in repr [:register reg] op val))
          (recur code repr))))))

(defn part-1
  "Day 08 Part 1"
  [input]
  (->> input
       u/to-lines
       (map u/tokenize)
       compile-code
       run-code
       :register
       vals
       (apply max)))

(defn- run-code-2
  "'Run' the code given"
  [repr]
  (loop [[ins & code] (:code repr), repr (assoc repr :maxval 0)]
    (if (nil? ins)
      repr
      (let [[op reg val cmp lhs rhs] ins]
        (if (cmp (get-in repr [:register lhs]) rhs)
          (let [value  (op (get-in repr [:register reg]) val)
                maxval (max value (:maxval repr))]
            (recur code (assoc (assoc-in repr [:register reg] value)
                               :maxval maxval)))
          (recur code repr))))))

(defn part-2
  "Day 08 Part 2"
  [input]
  (->> input
       u/to-lines
       (map u/tokenize)
       compile-code
       run-code-2
       :maxval))
