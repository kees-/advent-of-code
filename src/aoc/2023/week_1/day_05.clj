(ns aoc.2023.week-1.day-05
  (:require [aoc.util :as util]
            [clojure.string :as s]))

(defonce input (util/get-input 2023 5))

(defn ->nums
  [s]
  (->> (re-seq #"\d+" s)
       (map #(BigInteger. %))))

(def seeds
  (-> input
      (s/replace #"\n.*" "")
      ->nums))

(def maps
  (->> (util/s input #"\n\n")
       rest
       (mapv (comp #(sort-by second %)
                   #(map ->nums %)
                   #(util/s % #"\n")
                   #(s/replace % #"^.*:\s" "")))))

(defn find-interval
  [n m]
  (some #(when (<= n (+ (last %) (second %))) %) m))

(defn in-interval?
  [n [_ s l]]
  (<= s n (+' s l)))

(defn source->dest
  [n m]
  (let [[d s _] m]
    (if (in-interval? n m)
      (+ d (- n s))
      n)))

(defn mapper
  [n almanac]
  (loop [n n [this & remaining] almanac]
    (if-not this
      n
      (let [interval (find-interval n this)
            new-n (if interval (source->dest n interval) n)]
        (recur new-n remaining)))))

(->> seeds
     (map #(mapper % maps))
     (reduce min))
