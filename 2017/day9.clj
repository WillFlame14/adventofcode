(ns clojure-aoc.core (:gen-class))

(def input (slurp "input.txt"))

(defn parse [[score total_garbage depth garbage cancel] char]
  (if cancel
    [score total_garbage depth garbage false]
    (if garbage
      (if (= char \!)
        [score total_garbage depth true true]
        (let [stop (= char \>)]
          [score (+ (if stop 0 1) total_garbage) depth (not stop) false]))
      (case char
        \{ [score total_garbage (inc depth) false false]
        \} [(+ score depth) total_garbage (dec depth) false false]
        \< [score total_garbage depth true false]
        [score total_garbage depth false false]))))

  (defn -main
    [& args]
    (let [[part1 part2 & _] (reduce parse [0 0 0 false false] input)]
      (println (str part1 " " part2))))
