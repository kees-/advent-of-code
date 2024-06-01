(ns aoc.2023.week-1.day-03
  (:require [aoc.util :as util]
            [clojure.string :as s]))

(defonce input (util/get-input 2023 3))
(def sample
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defn is-symbol?
  [s]
  (not (re-find #"[\d\.]" (str s))))

(defn is-number?
  [s]
  (re-find #"\d" (str s)))

(defn row-symbols
  [v]
  (mapv is-symbol? v))

(defn indexed-row
  [row]
  (vec (map-indexed (fn [idx itm] [idx itm]) row)))

(defn row-symbol-indexes
  [[idx row]]
  (loop [agg [] [check & remaining] row]
    (if-not remaining
      [idx agg]
      (let [agg2 (if (second check)
                   (conj agg (first check))
                   agg)]
        (recur agg2 remaining)))))

(def data
  (->> input
       util/sl
       (map indexed-row)
       (map-indexed (fn [idx itm] [idx itm]))
       vec))

(def symbols
  (->> input
       util/sl
       (map row-symbols)
       (map indexed-row)
       (map-indexed (fn [idx itm] [idx itm]))
       (mapv row-symbol-indexes)))

(defn find-number-pos
  [row xpos]
  (loop [acc [] x xpos]
    (let [[pos value] (get (second row) x)
          num? (is-number? value)]
      (if-not num?
        acc
        (recur (conj acc pos)
               (inc x))))))

(defn find-numbers-in-row
  [[idx row]]
  (loop [acc [] ptr 0]
    (if-not (get row ptr)
      [idx acc]
      (let [trial (find-number-pos [idx row] ptr)
            next-acc (if (empty? trial)
                       acc
                       (conj acc trial))
            next-ptr (if (empty? trial)
                       (inc ptr)
                       (+ ptr (count trial)))]
        (recur next-acc next-ptr)))))

(defn neighbors-from-number-pos
  [[y xs]]
  (for [ys (vector (dec y) y (inc y))
        xss (reduce into [[(dec (first xs))] xs [(inc (last xs))]])
        :when (and (<= 0 ys) (<= 0 xss))]
    [ys xss]))

(defn coord-in-symbols?
  [syms [y x]]
  (some #(= x %) (get-in syms [y 1])))

(def coord-in-symbols?-closure (partial coord-in-symbols? symbols))

(defn any-coords-symbols?
  [syms coords]
  (let [f (fn [[y x]]
            (contains? (get-in syms [y 1]) x))]
    (some coord-in-symbols?-closure coords)))

(def any-coords-symbols?-closure (partial any-coords-symbols? symbols))

(defn number-has-symbol-neighbor?
  [[caterpillar _]]
  (-> caterpillar neighbors-from-number-pos any-coords-symbols?-closure))

(defn prep-for-num-checks
  [[y xs]]
  (mapv #(vector y %) xs))

(def all-numbers
  (let [f (fn [[y xs]]
            [[y xs]
             (->> (mapv #(get-in data [y 1 % 1]) xs)
                  (apply str)
                  util/str->int)])]
    (->> (mapv (fn [row]
                 (->> row find-numbers-in-row prep-for-num-checks))
               data)
         (reduce into)
         (mapv f))))

(->> all-numbers
     (filter number-has-symbol-neighbor?)
     (map second)
     (reduce +))

;; ========== try 2 ============================================================
(defn row-stars
  [v]
  (mapv #(= % \*) v))

(def stars
  (->> input
       util/sl
       (map row-stars)
       (map indexed-row)
       (map-indexed (fn [idx itm] [idx itm]))
       (mapv row-symbol-indexes)))

(defn y-xs-row->pairs
  [[y xs]]
  (for [x xs]
    [y x]))

(def all-gears-coords
  (->> stars
       (map y-xs-row->pairs)
       (reduce into [])))

(defn find-neighbors
  [[y x]]
  (for [y1 (range (dec y) (+ y 2))
        x1 (range (dec x) (+ x 2))
        :when (and (<= 0 y) (<= 0 x) (not= [y x] [y1 x1]))]
    [y1 x1]))

(defn get-data
  [[y x]]
  (get-in data [y 1 x 1]))

(defn number-neighbors
  [neighbors]
  (loop [acc #{} [[y x] & remaining] neighbors]
    (if-not remaining
      acc
      (recur (if (is-number? (get-data [y x]))
               (conj acc [y x]) acc)
             remaining))))

(defn foment-numbers
  [[y x]]
  (let [bck (loop [acc [] x1 (dec x)]
              (if-not (is-number? (get-data [y x1]))
                acc
                (recur (conj acc x1) (dec x1))))
        fwd (loop [acc [] x1 (inc x)]
              (if-not (is-number? (get-data [y x1]))
                acc
                (recur (conj acc x1) (inc x1))))
        goods (sort (reduce into [bck [x] fwd]))]
    (vec (for [x2 goods]
           [y x2]))))

(defn coords->numbers
  [sorted-coords]
  (->> sorted-coords
       (map get-data)
       s/join
       util/str->int))

(->> all-gears-coords
     (map (comp set
                #(map foment-numbers %)
                number-neighbors
                find-neighbors))
     (map #(map coords->numbers %))
     (filterv #(= 2 (count %)))
     (map #(apply * %))
     (reduce +))
