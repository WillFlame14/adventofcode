(ns clojure-aoc.core (:gen-class)
    (:require [clojure-aoc.tools :as tools]
              clojure.string))

(def queue clojure.lang.PersistentQueue/EMPTY)

(def input (slurp "input.txt"))

(defn get_val [regs s]
  (cond
    (nil? s) nil
    (nil? (re-matches #"-?\d+" s)) (or (get regs s) 0)
    :else (Integer/parseInt s)))

(defn step [regs cmd x_str y_str]
  (let [[x y] (map #(get_val regs %) [x_str y_str])]
    (cond
      (= cmd "set") (assoc regs x_str y)
      (= cmd "add") (assoc regs x_str (+ x y))
      (= cmd "mul") (assoc regs x_str (* x y))
      (= cmd "mod") (assoc regs x_str (mod x y)))))

(defn perform [instructions]
  (loop [i 0 regs {} last_snd 0]
    (if (or (< i 0) (>= i (count instructions)))
      (println (str "broke " regs))
      (let [[cmd x_str y_str] (clojure.string/split (instructions i) #" ")
            [x y] (map #(get_val regs %) [x_str y_str])]
        (println (instructions i))
        (cond
          (= cmd "snd") (recur (inc i) regs x)
          (= cmd "rcv") last_snd
          (= cmd "jgz") (recur (+ i (if (> x 0) y 1)) regs last_snd)
          :else (recur (inc i) (step regs cmd x_str y_str) last_snd))))))

(def part1 (perform (clojure.string/split input #"\n")))

(defn execute [r index instructions q]
  (loop [i index regs r rcv_q q snd_q queue]
    (if (or (< i 0) (>= i (count instructions)))
      [regs i snd_q]
      (let [[cmd x_str y_str] (clojure.string/split (instructions i) #" ")
            [x y] (map #(get_val regs %) [x_str y_str])]
        (cond
          (= cmd "snd") (recur (inc i) regs rcv_q (conj snd_q x))
          (= cmd "rcv") (if (empty? rcv_q)
                          [regs i snd_q]
                          (let [new_regs (assoc regs x_str (peek rcv_q))]
                            (recur (inc i) new_regs (pop rcv_q) snd_q)))
          (= cmd "jgz")  (recur (+ i (if (> x 0) y 1)) regs rcv_q snd_q)
          :else (recur (inc i) (step regs cmd x_str y_str) rcv_q snd_q))))))

(def part2
  (let [instructions (clojure.string/split input #"\n")]
    (loop [[a_regs ai a_q] (execute {"p" 0} 0 instructions [])
           [b_regs bi b_q] (execute {"p" 1} 0 instructions a_q)
           b_sent (count b_q)]
      (let [[new_ar ai2 snd_aq] (execute a_regs ai instructions b_q)]
        (if (empty? snd_aq)
          b_sent
          (let [[new_br bi2 snd_bq] (execute b_regs bi instructions snd_aq)]
            (recur [new_ar ai2 snd_aq] [new_br bi2 snd_bq] (+ b_sent (count snd_bq)))))))))

(defn -main [& args]
  (println (str part1 " " part2)))
