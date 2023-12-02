(ns aoc.2022.week-2.day-12-a-star-2
  (:require [aoc.util :as util]
            [aoc.week-2.day-12 :as d12]
            [clojure.string :as s]))

(def start [20 120])
(def end [20 0])
(def inf Double/POSITIVE_INFINITY)

(defn manhattan-distance
  [[y2 x2] [y1 x1]]
  (+ (abs ^Integer (- x2 x1))
     (abs ^Integer (- y2 y1))))

(defn surrounding-points
  "Return valid neighbors UDLR of given coord."
  [coord w h]
  (let [[y x] coord]
    (cond-> []
      (< 0 x) (conj (update coord 1 dec))
      (< 0 y) (conj (update coord 0 dec))
      (< x (- w 2)) (conj (update coord 1 inc))
      (< y (- h 2)) (conj (update coord 0 inc)))))

(defn f-score
  [coord start end]
  (let [[g h] (map #(manhattan-distance coord %) [start end])]
    (+ g h)))

(defn traversable?
  [grid coord target]
  (>= (inc (get-in grid target))
      (get-in grid coord)))

(defn rate-targets
  [coord]
  (fn [{:keys [parents fs gs] :as m} target]
    (let [g (+ (get gs coord inf)
               (manhattan-distance coord target))]
      (if (>= g (get gs target inf))
        m
        {:parents (assoc parents target coord)
         :gs (assoc gs target g)
         :fs (assoc fs target (+ (get gs target inf)
                                ( manhattan-distance end target)))}))))

(defn path
  [parents coord]
  (loop [c coord
         p [c]]
    (if (contains? parents c)
      (let [node (parents c)]
       (recur node (conj p node)))
      p)))

(defn a-star
  [value-grid]
  (let [w (count (first value-grid))
        h (count value-grid)]
    (loop [open #{start}
           closed #{}
           parents {}
           fs {start (f-score start start end)}
           gs {start 0}]
      (let [this (first (sort-by #(get fs % inf) open))]
        (if (= this end)
          (path parents this)
          (let [open (remove #{this} open)
                closed (conj closed this)
                targets (->> (surrounding-points this w h)
                             (remove closed)
                             (filter #(traversable? value-grid this %)))
                {:keys [parents gs fs]} (reduce (rate-targets this)
                                                {:parents parents
                                                 :gs gs
                                                 :fs fs}
                                                targets)]
            (recur (into open targets) closed parents fs gs)))))))

(comment
  (count (a-star d12/height-map))

  (->> (a-star d12/height-map)
       (reduce #(assoc-in %1 %2 9608) d12/height-map)
       (mapv #(s/join (mapv char %)))
       (s/join \newline)
       (spit "assets/12out.txt")))
