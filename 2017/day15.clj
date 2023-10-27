(ns clojure-aoc.core (:gen-class))

(defn generate_next [[a b]]
  [(-> a (* 16807) (mod 2147483647)) (-> b (* 48271) (mod 2147483647))])

(defn generate_next2 [[a b]]
  [(->> (iterate #(-> % (* 16807) (mod 2147483647)) a)
        next
        (filter #(= (mod % 4) 0))
        second)
   (->> (iterate #(-> % (* 48271) (mod 2147483647)) b)
        next
        (filter #(= (mod % 8) 0))
        first)])

(def part1
  (->> (iterate generate_next [289 629])
       (take 40000000)
       (filter (fn [[a b]] (= (mod a 65536) (mod b 65536))))
       count))

(def part2
  (->> (iterate generate_next2 [289 629])
       (take 5000000)
       (filter (fn [[a b]] (= (mod a 65536) (mod b 65536))))
       count))

(defn -main [& args]
  (println (str part1 " " part2)))
