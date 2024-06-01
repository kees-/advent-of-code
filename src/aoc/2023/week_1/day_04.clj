(ns aoc.2023.week-1.day-04
  (:require [aoc.util :as util]))

(defonce input (util/get-input 2023 4))
(def sample "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-numbers
  [[winners numbers]]
  [(mapv util/str->int (rest (re-seq #"\d+" winners)))
   (mapv util/str->int (re-seq #"\d+" numbers))])

(defn how-many-winners
  [[winners numbers]]
  (->> numbers
       (filter #(some #{%} winners))
       count))

(defn pow2
  [n]
  (if (zero? n)
    0
    (reduce * (repeat (dec n) 2))))

#_(->> input
       util/sl
       (map #(util/s % #"\|"))
       (map parse-numbers)
       (map how-many-winners)
       (map pow2)
       (reduce +))

(defn add-winner-data
  [{:keys [data] :as m}]
  (assoc m :winners (how-many-winners data)))

(def data
  (update-vals (->> input
                    util/sl
                    (map #(util/s % #"\|"))
                    (map parse-numbers)
                    (map #(assoc {:copies 1 :total-copies 1} :data %))
                    (zipmap (rest (range)))
                    (into (sorted-map)))
               add-winner-data))

(defn update-winner
  [state id]
  (-> state
      (update-in [id :copies] inc)
      (update-in [id :total-copies] inc)))

(def solved (let [c (count data)]
              (loop [card 1 state data]
                (if (< c card)
                  state
                  (let [w (get-in state [card :winners])
                        won (range (inc card) (min c (+ card 1 w)))
                        new-state (reduce update-winner state won)
                        final-state (update-in new-state [card :copies] dec)
                        next-card (if (<= (get-in state [card :copies]) 1) (inc card) card)]
                    (recur next-card final-state))))))

(->> solved
     vals
     (map :total-copies)
     (reduce +))
