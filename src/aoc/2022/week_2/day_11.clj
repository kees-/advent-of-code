(ns aoc.2022.week-2.day-11
  (:require [aoc.util :as util]
            [clojure.math :as math]))

(defonce input
  (util/get-input 2022 11))

(def regexes
  "Regexes for parsing data from each monkey string."
  (-> (vec (repeat 5 #"\d+"))
      (assoc 1 #"[\+\*]|\d+|old$")))

(def parsers
  "Functions for parsing monkey metadata."
  (into
   [#(mapv util/str->int %)
    #(-> (if (= ["*" "old"] %)
           [math/pow 2]
           [(eval (symbol (first %)))
            (util/str->int (second %))]))]
   (repeat 3 #(util/str->int (first %)))))

(def monkeys
  "A vector of maps for each monkey's metadata."
  (let [parse (fn [fs monkey]
                (mapv #(%1 %2) fs monkey))
        mon-keys [:items :op :test :t :f :n]]
    (->> (util/s input #"\n\n")
         (mapv (comp #(zipmap mon-keys (conj % 0))
                     #(parse parsers %)
                     #(mapv re-seq regexes %)
                     rest
                     util/sl)))))

;; (Same as LCM in given situation)
(def crt (->> monkeys (map :test) (reduce *)))

(defn decrease-worry
  "Minimize your worry about breaking items."
  [n part]
  (case part
    1 (/ n 3)
    2 (mod n crt)
    identity))

(defn inspect
  "Adjust the worry level of an item after primate inspection."
  [{:keys [monkey item part]}]
  (let [{:keys [op]} monkey
        operation ((fn [[o v]] #(o % v)) op)]
    (-> item operation (decrease-worry part) math/floor)))

(defn turn
  "Simulate a monkey throwing all their items away."
  [{:keys [monkeys id part]}]
  (let [monkey (monkeys id)]
    (loop [monkeys monkeys
           [item & remaining] (:items monkey)]
      (if item
        (let [{:keys [test t f]} monkey
              inspection (inspect {:monkey monkey :part part :item item})
              target (if (zero? (mod inspection test)) t f)]
          (recur (-> monkeys
                     (update-in [id :items] subvec 1)
                     (update-in [id :n] inc)
                     (update-in [target :items] conj inspection))
                 remaining))
        monkeys))))

(defn round
  "Simulate a single round of monkey business."
  [{:keys [monkeys part]}]
  (loop [m monkeys
         [id & remaining] (range (count monkeys))]
    (if id
      (recur (turn {:monkeys m
                    :id id
                    :part part})
             remaining)
      m)))

(defn play
  "Simulate all rounds specified and return level of monkey business."
  [monkeys {:keys [rounds part]}]
  (let [game (nth (iterate #(round {:part part :monkeys %}) monkeys)
                  rounds)]
    (->> game
         (map :n)
         sort
         reverse
         (take 2)
         (reduce *))))

(comment
  (play monkeys {:part 1 :rounds 20})
  ;; => 182293

  (play monkeys {:part 2 :rounds 10000})
  ;; => 54832778815
  )
