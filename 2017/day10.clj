(ns clojure-aoc.core (:gen-class)
    (:require clojure.string))

(def input (slurp "input.txt"))

(defn rotate [coll offset]
  (let [partition (mod offset (count coll))
        [initial rest] (split-at partition coll)]
    (concat rest initial)))

(defn knot [[rope start skip] len]
  (let [[knotted rest] (split-at len rope)
        shift (+ len skip)
        new_rope (-> (reverse knotted) (concat rest) (rotate shift))
        new_start (-> (count rope) (- shift) (+ start) (mod (count rope)))]
    [new_rope new_start (inc skip)]))

(defn knot_hash [lengths]
  (let [initial [(range 256) 0 0]
        [rope start] (reduce knot initial lengths)]
    (rotate rope start)))

(defn hex_string [rope]
  (let [sparse (->> (partition 16 rope)
                    (map #(apply bit-xor %)))]
    (apply str (map #(format "%x" %) sparse))))

(defn -main
  [& args]
  (let [part1 (let [lengths (map #(Integer/parseInt %) (clojure.string/split input #","))
                    [final1 final2] (knot_hash lengths)]
                (* final1 final2))
        part2 (let [ascii_input (map int (seq input))]
                (->> (concat ascii_input [17 31 73 47 23])
                     (repeat 64)
                     (apply concat)
                     knot_hash
                     hex_string))]
    (println (str part1 " " part2))))
