(ns aoc.week-1.day-03
  (:require [aoc.util :as util]
            [clojure.set :as set]))

(defonce input
  (util/get-input 3))

(defn cleave
  [c]
  (let [h (/ (count c) 2)]
    [(subvec c 0 h)
     (subvec c h)]))

(def v
  (merge
   (zipmap (map char (range 97 123))
           (range 1 27))
   (zipmap (map char (range 65 91))
           (range 27 53))))

(def i
  (->> input
       util/sl
       (map (comp #(apply set/intersection %)
                  #(mapv set %)
                  cleave
                  vec))))

(comment
  (->> i
       (mapv first)
       (mapv v)
       (reduce +))

  (->> input
       util/sl
       (partition 3)
       (map (comp
             #(->> % (apply set/intersection) first v)
             #(map (comp set seq) %)))
       (reduce +)))
