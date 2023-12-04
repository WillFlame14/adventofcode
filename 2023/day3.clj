(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(defn digit-indices [grid row]
  (let [line (nth grid row)
        reducer (fn [[acc [inds comb :as curr]] [ind char]]
                  (if (Character/isDigit char)
                    (let [new-comb (-> comb (* 10) (+ (- (int char) (int \0))))
                          new-inds (conj inds [ind row])]
                      [acc [new-inds new-comb]])
                    (if (empty? inds)
                      [acc curr]
                      [(conj acc curr) [[] 0]])))]
    (let [[result final] (->> (map-indexed vector line) (reduce reducer [[] [[] 0]]))]
      (if (empty? (first final))
        result
        (conj result final)))))

(defn regex-adjacent [grid loc regex]
  (let [ns (for [x (range -1 2) y (range -1 2)
                 :let [new-loc (map + loc [x y])]
                 :when (let [[x' y'] new-loc]
                         (and (< -1 y' (count grid))
                           (< -1 x' (count (nth grid y')))))]
             new-loc)
        valid? #(let [s (str (tools/at grid %))]
                  (re-matches regex s))]
    (filter valid? ns)))

(defn find-parts [grid]
  (->> (range (count grid))
    (map #(digit-indices grid %))
    (apply concat)
    (filter (fn [[locs _]] (some #(seq (regex-adjacent grid % #"[^0-9|\.]")) locs)))
    (map second)
    (apply +)))

(def part1
  (let [input (slurp "input.txt")]
    (find-parts (->> (clojure.string/split input #"\n") (map seq)))))

(defn compute-gears [grid]
  (let [reducer2 (fn [acc [locs val]]
                   (let [gears (distinct (apply concat (map #(regex-adjacent grid % #"\*") locs)))]
                     (reduce #(assoc %1 %2 (conj (or (get %1 %2) []) val)) acc gears)))]
    (->> (range (count grid))
      (map #(digit-indices grid %))
      (apply concat)
      (reduce reducer2 {})
      (filter #(= 2 (count (second %))))
      (map second)
      (map #(apply * %))
      (apply +))))

(def part2
  (let [input (slurp "input.txt")]
    (compute-gears (->> (clojure.string/split input #"\n") (map seq)))))

(defn -main [& args]
  (str part1 " " part2))
