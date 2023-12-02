(ns aoc.2023.week-1.day-02
  (:require [aoc.util :as util]
            [clojure.edn :as edn]))

(defonce input (util/get-input 2023 2))

(def data
  (let [->pair #(map edn/read-string (util/s % #"\s"))]
    (->> input
         util/sl
         (map (comp #(map ->pair %)
                    #(re-seq #"\d+\s\w" %))))))

(defn test-maxes
  [[n color]]
  (<= n ({'r 12 'g 13 'b 14} color)))

(comment
  ; 2771
  (->> data
       (map (comp #(every? true? %)
                  #(map test-maxes %)))
       (zipmap (rest (range)))
       (filter val)
       (map first)
       (reduce +))

  ; 70924
  (->> data
       (map (comp #(apply * (vals %))
                  #(util/pairs->map % (partial reduce max))))
       (reduce +)))
