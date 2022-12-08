(ns aoc.week-1.day-01
  (:require [clojure.java.io :as io]
            [aoc.util :as util]))

(comment
  (io/make-parents "assets/tmp")
  )

(defonce input
  (util/get-input 1))

(comment
  (->> (util/s input #"\n\n")
       (map (comp
             #(reduce + %)
             #(map util/str->int %)
             util/sl))
       sort
       reverse
       (take 3)
       (reduce +)))
