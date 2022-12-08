(ns aoc.week-1.day-05
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 5))

(defonce big-input
  (slurp "assets/05-big.txt"))

(defn drop-spaces
  [c]
  (remove #(= \space %) c))

(defn rearrange-crates
  [direction]
  (fn [crates [n from to]]
    (let [to-move (->> from crates (take n) direction)]
      (-> crates
          (update to #(reduce conj % to-move))
          (update from #(drop n %))))))

(let [[crates instructions] (map util/sl
                                 (util/s big-input #"\n\n"))
      crates (->> (butlast crates)
                  (map #(take-nth 4 (rest %)))
                  (apply map list)
                  (mapv drop-spaces))
      instructions (mapv (comp #(update % 2 dec)
                               #(update % 1 dec)
                               #(mapv util/str->int %)
                               #(re-seq #"\d+" %))
                         instructions)]
  (->> instructions
       (reduce (rearrange-crates identity) crates)
       (map first)
       (reduce str)))
