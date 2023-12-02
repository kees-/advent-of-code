(ns aoc.2022.week-2.day-13
  (:require [aoc.util :as util]
            [clojure.edn :as edn]))

(defonce input
  (util/get-input 2022 13))

(def packets
  (->> (util/s input #"\n+")
       (map edn/read-string)))

(def packet-pairs
  (partition 2 packets))

(defn compare-elements
  ([c] (when (= 2 (count c)) (apply compare-elements c)))
  ([l r]
   (let [[[lx lc ln] [rx rc rn]] (map (juxt number? coll? nil?) [l r])]
     (cond
       (and lx rx) (case (compare l r)
                     -1 true
                     1 false
                     nil)
       (and lc rc) (case (compare-elements (first l) (first r))
                     true true
                     false false
                     (compare-elements (next l) (next r)))
       (and ln rn) nil
       ln true
       rn false
       (and lx rc) (compare-elements [l] r)
       (and lc rx) (compare-elements l [r])))))

(defn index
  [results]
  (map-indexed #(conj [(inc %1)] %2) results))

(comment
  ;; Sum the indices of all correctly ordered packets
  (->> (map compare-elements packet-pairs)
       index
       (reduce #(if (%2 1) (+ (%2 0) %1) %1) 0))
  ;; => 5905

  ;; Sort all packets, insert [[2]] and [[6]], and multiply their indices
  (let [dividers [[[2]] [[6]]]]
    (->> (into packets dividers)
         (sort compare-elements)
         index
         (filter #(some #{(second %)} dividers))
         (reduce #(* (first %1) (first %2)))))
  ;; => 21691
  )
