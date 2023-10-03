(ns clojure-aoc.tools)

(defn findIndex [coll pred]
  (loop [i 0]
    (when (< i (count coll))
      (if (pred (nth coll i))
        i
        (recur (inc i))))))

(defn get_mode [coll]
  (first (apply (partial max-key second) (seq (frequencies coll)))))

