(ns aoc.2023.week-1.day-02
  (:require [aoc.util :as util]
            [clojure.edn :as edn]))

(defonce input
  (let [->pair #(mapv edn/read-string (util/s % #"\s"))]
    (->> (util/get-input 2023 2)
         util/sl
         (map (comp #(map ->pair %)
                    #(re-seq #"\d+\s\w" %))))))

(defn test-maxes
  [[n c]]
  (<= n ({'r 12 'g 13 'b 14} c)))

(defn high-color
  [v]
  (reduce max (map first v)))

(comment
  ; 2771
  (->> input
       (map (comp #(every? true? %)
                  #(map test-maxes %)))
       (zipmap (rest (range)))
       (filter val)
       (map first)
       (reduce +))

  ; 70924
  (->> input
       (map (comp #(apply * %)
                  vals
                  #(update-vals % high-color)
                  #(group-by second %)))
       (reduce +)))
