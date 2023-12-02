(ns aoc.2023.week-1.day-01
  (:require [aoc.util :as util]
            [clojure.string :as s]
            [clojure.edn :as edn]))

(def input (util/get-input 2023 1))

(def part-2
  (-> input
      (s/replace "one" "o1e")
      (s/replace "two" "t2o")
      (s/replace "three" "t3e")
      (s/replace "four" "f4r")
      (s/replace "five" "f5e")
      (s/replace "six" "s6x")
      (s/replace "seven" "s7n")
      (s/replace "eight" "e8t")
      (s/replace "nine" "n9e")))

(def digits
  {"one" 1 "two" 2 "three" 3
   "four" 4 "five" 5 "six" 6
   "seven" 7 "eight" 8 "nine" 9})

(defn text->int
  [s]
  (if-let [d (digits s)] d (edn/read-string s)))

(defn solve
  [data]
  (->> data
       util/sl
       (map (comp #(edn/read-string (str (first %) (last %)))
                  #(map text->int %)
                  #(re-seq #"\d" %)))
       (reduce +)))

(comment
  (solve input)
  ; 55208
  (solve part-2)
  ; 54578
  )
