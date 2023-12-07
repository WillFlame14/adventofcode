(ns clojure-aoc.core (:gen-class)
  (:require clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(def ranks (into {} (map vector (seq "23456789TJQKA") (range 0 13))))

(def hand-ranks (into {} (map vector [:high-card :one-pair :two-pair :3-kind :full-house :4-kind :5-kind] (range 0 7))))

(defn hand-type [hand]
  (let [freqs (sort-by second > (frequencies hand))
        max-freq (second (first freqs))]
    (cond
      (= max-freq 5) :5-kind
      (= max-freq 4) :4-kind
      (= max-freq 3) (if (= (second (second freqs)) 2) :full-house :3-kind)
      (= max-freq 2) (if (= (second (second freqs)) 2) :two-pair :one-pair)
      :else :high-card)))

(def ranks2 (into {} (map vector (seq "J23456789TQKA") (range 0 13))))

(defn joker-type [hand]
  (let [type (hand-type hand)
        jokers (count (filter #(= % \J) hand))]
    (cond
      (= jokers 0) type
      (some #(= % type) [:5-kind :4-kind :full-house]) :5-kind
      (= type :3-kind) :4-kind
      (= type :two-pair) (if (= jokers 2) :4-kind :full-house)
      (= type :one-pair) :3-kind
      (= type :high-card) :one-pair)))

(defn compare-hands [jokers?]
  (fn [hand1 hand2]
    (let [[type1 type2] (map (if jokers? joker-type hand-type) [hand1 hand2])]
      (cond
        (< (hand-ranks type1) (hand-ranks type2)) -1
        (> (hand-ranks type1) (hand-ranks type2)) 1
        :else (loop [i 0]
                (if (= i 5)
                  0
                  (let [[i1 i2] (map #((if jokers? ranks2 ranks) (nth % i)) [hand1 hand2])]
                    (cond
                      (< i1 i2) -1
                      (> i1 i2) 1
                      :else (recur (inc i))))))))))

(defn winnings [ordered-hands]
  (->> ordered-hands
    (map second)
    (map-indexed vector)
    (map (fn [[ind bid]] (* (inc ind) (Integer/parseInt bid))))
    (apply +)
    ))

(def part1
  (let [hands (map #(clojure.string/split % #" ") input)
        ordered (sort-by first (compare-hands false) hands)]
    (winnings ordered)))

(def part2
  (let [hands (map #(clojure.string/split % #" ") input)
        ordered (sort-by first (compare-hands true) hands)]
    (winnings ordered)))

(defn -main [& args]
  (str part1 " " part2))

(-main)
