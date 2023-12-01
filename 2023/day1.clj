(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(defn strip [line]
  (let [digits (->> (seq line) (filter #(Character/isDigit %)))]
    (->> [(first digits) (last digits)]
      (apply str)
      Integer/parseInt)))

(def nums ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(defn strip2 [line]
  (let [digits (re-seq #"(?=([1-9]|one|two|three|four|five|six|seven|eight|nine))" line)]
    (->> [(first digits) (last digits)]
      (map second)
      (map (fn [dig] (if (= 1 (count dig))
                       dig
                       (tools/find-index nums #(= % dig)))))
      (apply str)
      Integer/parseInt)))

(def part1
  (let [input (slurp "input.txt")]
    (->> (clojure.string/split input #"\n")
      (map strip)
      (apply +))))

(def part2
  (let [input (slurp "input.txt")]
    (->> (clojure.string/split input #"\n")
      (map strip2)
      (apply +))))

(defn -main [& args]
  (str part1 " " part2))
