(ns clojure-aoc.core (:gen-class))

(defn captcha
  ([input]
    (captcha (str input (first input)) 0)
  )
  ([[x & xs] acc]
    (if (empty? xs)
      acc
      (recur xs (if (= x (first xs)) (+ (- (int x) (int \0)) acc) acc))
    )
  )
)

(defn captcha2
  [input]
  (let [len (count input)]
    (loop [i 0 sum 0]
      (if (< i len)
        (do
          (defn match [in] (= (get input in) (get input (mod (+ in (/ len 2)) len))))
          (def value (- (int (get input i)) (int \0)))
          (recur (inc i) (if (match i) (+ sum value) sum))
        )
        sum
      )
    )
  )
)

(defn -main
  [& args]
  (do
    (def input (slurp "input.txt"))
    (str (captcha input) " " (captcha2 input))
  )
)