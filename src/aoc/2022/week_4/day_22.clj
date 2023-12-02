(ns aoc.2022.week-4.day-22
  (:require [aoc.util :as util]
            [clojure.string :as s]))

(defonce inputs
  (util/s (util/get-input 2022 22) #"\n\n"))

(defonce samples
  (util/s (slurp "assets/22sample.txt") #"\n\n"))

(def board
  (let [parse #(case % \. :open \# :wall nil)]
    (->> inputs first util/sl (mapv #(mapv parse (seq %))))))

(def empty-board
  (->> inputs first util/sl (mapv vec)))

(def instructions
  (let [parse #(case % "L" :left "R" :right (util/str->int %))]
    (->> inputs second (re-seq #"\d+|\w") (map parse))))

(def hard
  {:right {:u :r :r :d :d :l :l :u}
   :left {:u :l :l :d :d :r :r :u}})

(def dirs {:u [0 -1] :d [0 1] :l [1 -1] :r [1 1]})

(defn make-path
  [coord facing l]
  (let [[idx mult] (facing dirs)
        m (coord idx)]
    (for [n (range m (+ m (* mult (inc l))) mult)]
      (assoc coord idx n))))

(defn find-wrap
  "If a movement would take the pos off the board, wrap it around."
  [board coords facing]
  (let [[[a b] & remaining] coords
        c (count remaining)
        wrap (case facing
               :u (->> (map #(vector % b) (range (count board)))
                       (remove #(nil? (get-in board %)))
                       reverse
                       first)
               :d (->> (map #(vector % b) (range (count board)))
                       (some #(when (get-in board %) %)))
               :l (->> (map #(vector a %) (range (count (board a))))
                       (remove #(nil? (get-in board %)))
                       reverse
                       first)
               :r (->> (map #(vector a %) (range (count (board a))))
                       (some #(when (get-in board %) %))))]
    (make-path wrap facing c)))

(defn free-space
  "Move forward while not a wall."
  [board coords]
  (take-while #(not= (get-in board %) :wall) coords))

(defn move
  "Given a linear series of coordinates and a direction,
   give the possible final position on the board."
  [board coords facing]
  (let [extent (free-space board coords)
        wrapped (drop-while #(some? (get-in board %)) extent)]
    (if (seq wrapped)
      (let [remaining (find-wrap board wrapped facing)]
        (if-let [p (last (free-space board remaining))]
          p (first coords)))
      (last extent))))

(defn update-pos
  "Returns a reducer that applies one instruction of any type to a board state."
  [board debug?]
  (fn [[pos facing #_empty-board] instruction]
    (when debug? (println [pos facing] instruction))
    (if (keyword? instruction)
      [pos (get-in hard [instruction facing]) #_(assoc-in empty-board pos \█)]
      (let [path (make-path pos facing instruction)
            new-pos (move board path facing)]
        [new-pos facing #_(assoc-in empty-board new-pos \█)]))))

(def start [0 50])

(comment
  (reduce (update-pos board false) [start :r #_empty-board] instructions)
  (reduce (update-pos board true) [start :r empty-board] (take 100 instructions))

  (->> (reduce (update-pos board false)
               [start :r (->> inputs first util/sl (mapv vec))]
               instructions)
       last
       (mapv s/join)
       (s/join \newline)
       (spit "assets/boop.txt"))

  (+ (* 1000 160)
     (* 4 7)
     0))
