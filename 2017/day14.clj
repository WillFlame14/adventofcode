(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string
              clojure.set))

(def input (slurp "input.txt"))

(defn knot [[rope start skip] len]
  (let [[knotted rest] (split-at len rope)
        shift (+ len skip)
        new_rope (-> (reverse knotted) (concat rest) (tools/rotate shift))
        new_start (-> (count rope) (- shift) (+ start) (mod (count rope)))]
    [new_rope new_start (inc skip)]))

(defn knot_hash_fn [lengths]
  (let [initial [(range 256) 0 0]
        [rope start] (reduce knot initial lengths)]
    (tools/rotate rope start)))

(defn dense_hash [rope]
  (->> (partition 16 rope)
       (map #(apply bit-xor %))))

(defn knot_hash [input]
  (let [ascii_input (map int (seq input))]
    (->> (concat ascii_input [17 31 73 47 23])
         (repeat 64)
         (apply concat)
         knot_hash_fn
         dense_hash)))

(defn binary_hash [knot_hash]
  (let [pad_binary #(->> (Integer/toString % 2)
                         Integer/parseInt
                         (format "%08d"))]
    (apply str (map pad_binary knot_hash))))

(defn squares [input]
  (->> (knot_hash input) binary_hash (filter #(= \1 %)) count))

(def part1
  (reduce #(+ %1 (squares (str input "-" %2))) 0 (range 128)))

(defn get_disk [input]
  (map #(-> (str input "-" %) knot_hash binary_hash) (range 128)))

(defn find_connected [start disk]
  (loop [visited #{start} to_visit [start]]
    (if (empty? to_visit)
      visited
      (let [curr (peek to_visit)
            valid (fn [[x y]] (and
                               (<= 0 x 127) (<= 0 y 127)
                               (= \1 (tools/at disk [x y]))
                               (not (contains? visited [x y]))
                               (not (contains? to_visit [x y]))))
            neighbours (map #(map + curr %) [[-1 0] [0 1] [1 0] [0 -1]])
            valid_n (filter valid neighbours)]
        (recur (conj visited curr) (into (pop to_visit) valid_n))))))

(def disk (get_disk input))

(def part2
  (loop [[x y] '(0 0) regions 0 visited #{}]
    (if (= x y (dec (count disk)))
      regions
      (let [next_loc (if (= x (dec (count disk))) `(0 ~(inc y)) `(~(inc x) ~y))]
        (if (or (= (tools/at disk [x y]) \0) (contains? visited `(~x ~y)))
          (recur next_loc regions visited)
          (let [connected (find_connected `(~x ~y) disk)]
            (recur next_loc (inc regions) (into visited connected))))))))

(defn -main [& args]
  (println (str part1 " " part2)))
