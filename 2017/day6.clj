(ns clojure-aoc.core (:gen-class))

(defn maxIndex [f coll]
  (loop [i 1 curr_max (f (first coll)) curr_max_i 0]
    (if (< i (count coll))
      (let [curr (f (nth coll i))
            new_max? (> curr curr_max)]
          (recur (inc i) (if new_max? curr curr_max) (if new_max? i curr_max_i))
        )
      curr_max_i
      )
    )
  )

(defn redistribute [blocks]
  (let [max_index (maxIndex identity blocks)
        max_block (get blocks max_index)
        len (count blocks)
        quotient (quot max_block len)
        remainder (mod max_block len)
        start_index (mod (inc max_index) len)
        ]
    (loop [i 0 new_blocks (assoc blocks max_index 0)]
      (let [index (mod (+ i start_index) len)
            incr (if (< i remainder) (inc quotient) quotient)]
        (if (< i len)
          (recur (inc i) (assoc new_blocks index (+ (get new_blocks index) incr)))
          new_blocks
          )
        )
      )
    )
  )

(defn duplicate-reducer [[acc i] curr]
  (if (contains? acc curr)
    (reduced i)
    [(conj acc curr) (inc i)])
  )

(defn first-duplicate-index [coll]
  (reduce duplicate-reducer [#{} 0] coll)
  )

(defn first-cycle-length [coll]
  (let [duplicate (nth coll (first-duplicate-index coll))]
    (first-duplicate-index (iterate redistribute duplicate))
    )
  )

(defn -main
  [& args]
    (let [input (->> (clojure.string/split (slurp "input.txt") #"\t")
                     (map #(Integer/parseInt %))
                     (into []))
          part1 (first-duplicate-index (iterate redistribute input))
          part2 (first-cycle-length (iterate redistribute input))]
      (println (str part1 " " part2))
      )
)
