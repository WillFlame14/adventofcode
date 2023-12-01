(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (slurp "input.txt"))

(defn get-val [regs str]
  (if (re-matches #"-?\d+" str)
    (Integer/parseInt str)
    (get regs str)))

(defn run [regs [cmd x y] i]
  (let [[x-val y-val] (map #(get-val regs %) [x y])]
    (cond
      (= cmd "set") [(assoc regs x y-val) (inc i)]
      (= cmd "sub") [(assoc regs x (- x-val y-val)) (inc i)]
      (= cmd "mul") [(assoc regs x (* x-val y-val)) (inc i)]
      (= cmd "jnz") (if (not= x-val 0)
                      [regs (+ i y-val)]
                      [regs (inc i)]))))

(def part1
  (let [insts (clojure.string/split input #"\n")
        init-regs (->> (seq "abcdefgh") (map str) (reduce #(assoc %1 %2 0) {}))]
    (loop [i 0 regs init-regs muls 0]
      (if (< i (count insts))
        (let [inst-parts (clojure.string/split (nth insts i) #" ")
              [new-regs new-i] (run regs inst-parts i)
              new-muls (if (= (first inst-parts) "mul") (inc muls) muls)]
          (recur new-i new-regs new-muls))
        muls))))

(defn prime? [n]
  (->> (range 2 (inc (int (Math/sqrt n))))
       (filter #(= 0 (mod n %)))
       empty?))

(def part2
  (->> (range 109900 126917 17)
       (filter #(not (prime? %)))
       count))

(defn -main []
  (println (str part1 " " part2)))
