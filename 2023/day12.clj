(ns clojure-aoc.core (:gen-class)
    (:require clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(def nonogram
  (memoize
   (fn [line run reqs]
     (cond
       (empty? reqs) (if (every? #(or (= % \.) (= % \?)) line) 1 0)
       (empty? line) (if (and (= (count reqs) 1) (= run (first reqs))) 1 0)
       (> run (first reqs)) 0
       :else (let [char (first line)
                   line' (rest line)
                   dot-reqs (if (= run (first reqs)) (rest reqs) reqs)]
               (cond
                 (= char \.) (if (and (not= run 0) (< run (first reqs)))
                               0
                               (recur line' 0 dot-reqs))
                 (= char \#) (recur line' (inc run) reqs)
                 :else (cond
                         (= run (first reqs)) (recur line' 0 dot-reqs)
                         (< run (first reqs)) (if (not= run 0)
                                                (recur line' (inc run) reqs)
                                                (+ (nonogram line' 0 dot-reqs)
                                                   (nonogram line' (inc run) reqs))))))))))

(defn parse-line [line]
  (let [[n r] (clojure.string/split line #" ")
        reqs (->> (clojure.string/split r #",") (map #(Integer/parseInt %)))]
    (nonogram (seq n) 0 reqs)))

(defn parse-line2 [line]
  (let [[n r] (clojure.string/split line #" ")
        nono (->> (seq n) (repeat 5) (interleave (repeat \?)) next flatten)
         reqs (->> (clojure.string/split r #",") (map #(Integer/parseInt %)) (repeat 5) flatten)]
    (nonogram nono 0 reqs)))

(def part1 (->> input (map parse-line) (apply +)))

(def part2 (->> input (map parse-line2) (apply +)))

(defn -main [& args]
  (str part1 " " part2))