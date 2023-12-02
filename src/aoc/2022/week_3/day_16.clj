(ns aoc.2022.week-3.day-16
  (:require [aoc.util :as util]))

(defonce input
  (util/get-input 2022 16))

(def graph-description
  (->> input
       util/sl
       (map #(vec (re-seq #"[A-Z]{2}|\d+" %)))
       (map #(update % 1 util/str->int))))

(defn node
  [[valve flow & connections]]
  {valve {:flow flow
          :conn connections}})

(def graph
  (reduce #(into %1 (node %2))
          (sorted-map)
          graph-description))

(defn add-distances
  [graph node]
  (let [{:keys [dist conn]} (graph node)
        new-dist (if dist (inc dist) 0)]
    (cond-> graph
      (not dist) (assoc-in [node :dist] new-dist))
    (as-> graph g
      (assoc-in g [node :dist] new-dist)
      (reduce add-distances g conn))
    graph))

(add-distances (assoc-in graph ["AA" :dist] 0) "AA")
