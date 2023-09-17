(ns clojure-aoc.core
  (:gen-class))

(defn no_duplicates [parts]
  (= (count parts) (count (set parts)))
  )

(defn no_anagrams [parts]
  (loop [i 0]
    (let [x (get parts i)
          anagram_found (some #(= (frequencies x) (frequencies %)) (drop (inc i) parts))]
      (if anagram_found
        false
        (if (< i (count parts))
          (recur (inc i))
          true
          )
        )
      )
    )
  )

(defn -main
  [& args]
  (let [input (clojure.string/split (slurp "input.txt") #"\n")
        lines (map #(clojure.string/split % #" ") input)
        part1 (->> lines
                (filter no_duplicates)
                count
                )
        part2 (->> lines
                (filter no_anagrams)
                count
                )
        ]
    (str part1 " " part2)
    )
  )
