(ns clojure-aoc.core (:gen-class))

(defn diff
  [line]
  (do
    (def parts (clojure.string/split line #"\t"))
    (def nums (into [] (map #(Integer/parseInt %) parts)))
    (- (apply max nums) (apply min nums))
  )
)

(defn div
  [line]
  (do
    (def parts (clojure.string/split line #"\t"))
    (def nums (into [] (map #(Integer/parseInt %) parts)))
    (loop [i 0] (when (< i (count nums))
      (let [ret (loop [j (+ i 1)] (when (< j (count nums))
        (let [
          [x y] (map (partial get nums) [i j])
          big (max x y)
          small (min x y)
        ]
          (if (= (mod big small) 0)
            (/ big small)
            (recur (inc j))
          )
        )
      ))]
        (if (nil? ret)
          (recur (inc i))
          ret
        )
      )
    ))
  )
)

(defn -main
  [& args]
  (do
    (def input (slurp "input.txt"))
    (def part1 (reduce + (map #(diff %) (clojure.string/split input #"\n"))))
    (def part2 (reduce + (map #(div %) (clojure.string/split input #"\n"))))
    (println (str part1 " " part2))
  )
)
