(ns clojure-aoc.core (:gen-class))

(defn maze [initial_offsets start_index part2]
  (loop [i 0 index start_index offsets initial_offsets]
    (let [curr (get offsets index)
          next_index (+ index curr)
          op (if (and part2 (>= curr 3)) dec inc)
          next_offsets (assoc offsets index (op curr))]
      (if (or (< next_index 0) (>= next_index (count offsets)))
        (inc i)
        (recur (inc i) next_index next_offsets)
        )
      )
    )
  )

(defn -main
  [& args]
    (let [input (->> (clojure.string/split (slurp "input.txt") #"\n")
                     (map #(Integer/parseInt %))
                     (into []))
          part1 (maze input 0 false)
          part2 (maze input 0 true)]
      (println (str part1 " " part2))
      )
)
