(ns aoc.2023.week-1.day-01
  (:require [aoc.util :as util]
            [clojure.string :as s]))

(defonce input (util/get-input 2023 1))

(defn solve
  [data]
  (->> data
       util/sl
       (map (comp #(util/str->int (str (first %) (last %)))
                  #(re-seq #"\d" %)))
       (reduce +)))

(comment
  ; 55208
  (solve input)
  ; 54578
  (solve (-> input
             (s/replace "one" "o1e")
             (s/replace "two" "t2o")
             (s/replace "three" "t3e")
             (s/replace "four" "4")
             (s/replace "five" "5e")
             (s/replace "six" "6")
             (s/replace "seven" "7n")
             (s/replace "eight" "e8t")
             (s/replace "nine" "n9e"))))
