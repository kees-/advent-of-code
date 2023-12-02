(ns aoc.2022.week-2.day-12-a-star
  (:require [aoc.util :as util]
            [aoc.week-2.day-12 :as d12]
            [clojure.pprint :refer [pprint]]))

(def start [20 0])
(def end [20 120])

(defn manhattan-distance
  [[y2 x2]]
  (fn [[y1 x1]]
    (+ (abs ^Integer (- x2 x1))
       (abs ^Integer (- y2 y1)))))

(defn node
  [coord start end]
  (let [[y x] coord]
    {:open? true}))

;; (defn blank-map
;;   [grid start end]
;;   (let [w (count (first grid))
;;         h (count grid)]
;;     (vec (for [y (range h)
;;                :let [row (mapv #(node [y %] start end)
;;                                (range w))]]
;;            row))))

;; (get-in (blank-map d12/height-map start end) [30 133])

(defn traversable?
  [grid nodes coord target]
  (and (get-in grid target)
       (:open? (get-in nodes target))
       (>= (inc (get-in grid coord))
           (get-in grid target))))

;; (traversable? d12/height-map
;;               (blank-map d12/height-map start end)
;;               [6 120]
;;               [6 119])
;; (get-in d12/height-map [6 119])
;; (get-in d12/height-map [6 120])

;; (defn parse-node
;;   [grid coord]
;;   (fn [nodes target]
;;     (cond-> nodes
;;       (traversable? grid nodes coord target)
;;       (assoc-in))))

;; (defn a-step
;;   [grid nodes coord]
;;   (let [w (count (first grid))
;;         h (count grid)
;;         targets (d12/surrounding-points coord w h)]
;;     targets))

;; (a-step d12/height-map
;;         (blank-map d12/height-map start end)
;;         [6 119])
