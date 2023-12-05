(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-line [line]
  (let [[d-start s-start rng] (->> (clojure.string/split line #" ") (map #(Long/parseLong %)))]
    {:d-start d-start
     :s-start s-start
     :rng rng}))

(defn parse-map [lines]
  (let [[source _ destination] (-> (clojure.string/split (first lines) #" ")
                                   first
                                   (clojure.string/split #"-"))
        mappings (reduce #(conj %1 (parse-line %2)) [] (next lines))]
    [source {:dest destination
             :mappings mappings}]))

(defn get-transform [source val transforms]
  (let [{dest :dest mappings :mappings} (transforms source)]
    (loop [i 0]
      (if (< i (count mappings))
        (let [{d-start :d-start s-start :s-start rng :rng} (nth mappings i)]
          (if (<= s-start val (+ s-start (dec rng)))
            [dest (+ d-start (- val s-start))]
            (recur (inc i))))
        [dest val]))))

(def part1
  (let [seeds (map #(Long/parseLong %) (-> (clojure.string/split (first input) #": ")
                                           second
                                           (clojure.string/split #" ")))
        transforms (->> (drop 2 input) (tools/split-all #(= "" %)) (map parse-map) (into {}))]
    (loop [type "seed" vals seeds]
      (if (= type "location")
        (apply min vals)
        (let [results (map #(get-transform type % transforms) vals)]
          (recur (first (first results)) (map second results)))))))

(defn get-reverse [dest val transforms]
  (let [[source {mappings :mappings}] (first (filter (fn [[_ {d :dest}]] (= d dest)) (seq transforms)))]
    (loop [i 0]
      (if (< i (count mappings))
        (let [{d-start :d-start s-start :s-start rng :rng} (nth mappings i)]
          (if (<= d-start val (+ d-start (dec rng)))
            [source (+ s-start (- val d-start))]
            (recur (inc i))))
        [source val]))))

(defn compute-reverse [val transforms]
  (loop [type "location" val val]
    (if (= type "seed")
      val
      (let [[new-type new-val] (get-reverse type val transforms)]
        (recur new-type new-val)))))

(defn in-seed-range [seeds val]
  (some (fn [[start rng]] (<= start val (+ start (dec rng)))) seeds))

;; 3 hour runtime baybee
(def part2
  (let [seeds (->> (-> (clojure.string/split (first input) #": ")
                       second
                       (clojure.string/split #" "))
                   (map #(Long/parseLong %))
                   (partition 2))
        transforms (->> (drop 2 input)
                        (tools/split-all #(= "" %))
                        (map parse-map)
                        (into {}))]
    (loop [i 0]
      ;; periodic print to check that we're not stuck
      (if (zero? (mod i 1000000))
        (println i)
        ())
      (let [val (compute-reverse i transforms)]
        (if (in-seed-range seeds val)
          i
          (recur (inc i)))))))

(defn -main [& args]
  (str part1 " " part2))
