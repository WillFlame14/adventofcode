(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (clojure.string/split (slurp "input.txt") #"\n"))

(defn process_cmd [regs line]
  (let [[r o v _ cr cmp cv] (clojure.string/split line #" ")
        reg (or (get regs r) 0)
        op (if (= o "inc") + -)
        val (Integer/parseInt v)
        cond_reg (or (get regs cr) 0)
        comp (if (= cmp "==") = (if (= cmp "!=") not= (resolve (symbol cmp))))
        cond_val (Integer/parseInt cv)]
    (if (comp cond_reg cond_val)
      (assoc regs r (op reg val))
      regs)))

(defn reducer [[prev_max regs] line]
  (let [next_regs (process_cmd regs line)
        curr_max (apply max (vals next_regs))]
    [(max curr_max prev_max) next_regs]))

(defn -main
  [& args]
  (let [part1 (->> (reduce process_cmd {} input)
                   vals
                   (apply max))
        part2 (->> (reduce reducer [0 {}] input)
                   first)]
    (println (str part1 " " part2))))
