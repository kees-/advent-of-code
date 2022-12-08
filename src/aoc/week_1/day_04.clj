(ns aoc.week-1.day-04
  (:require [aoc.util :as util]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(defonce input
  (util/get-input 4))

(defn ranges
  [[a b c d]]
  [(set (range c (inc d)))
   (set (range a (inc b)))])

(->> input
     util/sl
     (mapv (comp
            #(apply set/difference %)
            (partial sort #(< (count %1) (count %2)))
            ranges
            #(mapv edn/read-string %)
            #(re-seq #"\d+" %)))
     (filter empty?)
     count)

(->> input
     util/sl
     (mapv (comp
            #(apply set/intersection %)
            #_(partial sort #(< (count %1) (count %2)))
            ranges
            #(mapv edn/read-string %)
            #(re-seq #"\d+" %)))
     (remove empty?)
     count)
