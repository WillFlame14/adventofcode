(ns clojure-aoc.core
  (:gen-class))

(defn get_start [input]
  (loop [i 0 addend 1 sum 1]
    (let [next (+ sum addend)]
      (if (or (< next input) (= next input))
        (recur (inc i) (+ addend 8) next)
        `(~sum ~i)
        )
      )
    )
  )

(def dirs ['(0 1) '(-1 0) '(0 -1) '(1 0)])

(defn pair [start end x]
  (loop [diff (- end start) dir_index 0 point `(~x 0)]
    (let [max_dist (if (= dir_index 0) x (* 2 x))
          travel (min diff max_dist)
          new_point (->> (get dirs dir_index)
                      (map #(* travel %))
                      (map + point)
                      )
          remaining (- diff travel)
          ]
      (if (> remaining 0)
        (recur remaining (mod (inc dir_index) 4) new_point)
        new_point
        )
      )
    )
  )

(def adjs ['(-1 1) '(0 1) '(1 1) '(-1 0) '(1 0) '(-1 -1) '(0 -1) '(1 -1)])

(defn sum_adjs [table point]
  (loop [i 0 sum 0]
    (if (< i (count adjs))
      (let [adj (map + point (get adjs i))
            adj_val (get table adj)
            new_sum (if (nil? adj_val) sum (+ sum adj_val))
            ]
        (recur (inc i) new_sum)
        )
      sum
      )
    )
  )

(defn get_larger [input]
  (loop [table (assoc {} '(0 0) 1) [x y :as point] '(1 0) dir_index 0]
    (let [val (sum_adjs table point)]
      (if (> val input)
        val
        (let [at_corner (= (Math/abs x) (+ (if (= dir_index 3) 1 0) (Math/abs y)))
              next_dir_index (if at_corner (mod (inc dir_index) 4) dir_index)
              next_pt (map + point (get dirs next_dir_index))
              ]
          (recur (assoc table point val) next_pt next_dir_index)
          )
        )
      )
    )
  )

(defn -main
  [& args]
  (let [input (Integer/parseInt (slurp "input.txt"))
        [start x] (get_start input)
        final_pt (pair start input x)
        part1 (apply + (map #(Math/abs %) final_pt))
        part2 (get_larger input)
        ]
    (str part1 " " part2)
    )
  )
