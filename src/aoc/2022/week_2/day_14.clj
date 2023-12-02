(ns aoc.2022.week-2.day-14
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 14))

(defn pair->line
  [pair]
  (let [[[x1 y1] [x2 y2]] pair
        delta (apply map - pair)
        dir (if (some neg? delta) -1 1)
        axis (if (zero? (first delta)) 0 1)]
    #_(for [m (range (if ))])))

(def lines
  (->> (util/sl input)
       (mapv (comp #(partition 2 %)
                   #(partition 2 %)
                   #(map util/str->int %)
                   #(re-seq #"\d+" %)))))

(pair->line (ffirst lines))
