(ns clojure-aoc.core (:gen-class)
  (:require [clojure-aoc.tools :as tools]
    clojure.string))

(def input (-> (slurp "input.txt") (clojure.string/split #"\n")))

(defn parse-module [line]
  (let [[_ type name o] (re-matches #"([&%]?)(\w+) -> ([\w ,]+)" line)
        outputs (clojure.string/split o #", ")]
    (cond
      (= name "broadcaster") [name {:type :bc :outputs outputs}]
      (= type "%") [name {:type :ff :state :off :outputs outputs}]
      (= type "&") [name {:type :cj :memory {} :outputs outputs}]
      :else (println (str "parsing error " [type name o])))))

(defn initialize [modules]
  (let [reducer (fn [acc-mods curr-mod]
                  (let [[source {outputs :outputs}] curr-mod]
                    (reduce (fn [mods dest]
                              (if-let [{type :type memory :memory} (mods dest)]
                                (if (= type :cj)
                                  (assoc-in mods [dest :memory] (assoc memory source :low))
                                  mods)
                                mods)) acc-mods outputs)))]
    (reduce reducer modules modules)))

(defn process-signal [modules [signal source dest]]
  (if-let [{type :type, outputs :outputs, state :state, memory :memory} (modules dest)]
    (cond
      (= type :bc) [modules (map #(vector signal dest %) outputs)]
      (= type :ff) (if (= signal :high)
                     [modules []]
                     [(assoc-in modules [dest :state] (if (= state :on) :off :on))
                      (map #(vector (if (= state :on) :low :high) dest %) outputs)])
      (= type :cj) (let [new-memory (assoc memory source signal)
                         out-signal (if (every? (fn [[_ s]] (= s :high)) new-memory) :low :high)]
                     [(assoc-in modules [dest :memory] new-memory) (map #(vector out-signal dest %) outputs)]))
    [modules []]))

(defn press-button [modules]
  (loop [mods modules signals (tools/queue [[:low :start "broadcaster"]]) low-pulses 0 high-pulses 0]
    (if-let [[signal source dest :as s] (peek signals)]
      (do (if (and (= source "zx") (= signal :high)) (println (str "dr sending " signal)))
        (let [[new-mods new-signals] (process-signal mods s)]
          (recur new-mods (into (pop signals) new-signals) (if (= signal :low) (inc low-pulses) low-pulses) (if (= signal :high) (inc high-pulses) high-pulses))))
      [mods low-pulses high-pulses])))

(def part1
  (let [modules (initialize (into {} (map parse-module input)))
        iterations (iterate (fn [[mods lows highs]]
                              (let [[m l h] (press-button mods)] [m (+ lows l) (+ highs h)]))
                     [modules 0 0])
        [lows highs] (next (nth iterations 1000))]
    (* lows highs)))

(defn find-first [modules name]
  (loop [mods modules signals (tools/queue) presses 0]
    (if-let [[signal source _ :as s] (peek signals)]
      (if (and (= source name) (= signal :high))
        presses
        (let [[new-mods new-signals] (process-signal mods s)]
          (recur new-mods (into (pop signals) new-signals) presses)))
      (recur mods (conj signals [:low :start "broadcaster"]) (inc presses)))))

;; Find all the conjunction modules that feed into rx, then find their loop cycles when they send high pulses.
(def part2
  (let [modules (initialize (into {} (map parse-module input)))]
    (->> (map #(find-first modules %) ["dr" "vn" "ln" "zx"])
      (apply tools/lcm))))

(defn -main [& args]
  (println (str part1 " " part2)))
