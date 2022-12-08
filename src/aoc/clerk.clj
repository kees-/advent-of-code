(ns aoc.clerk
  (:require [nextjournal.clerk :as clerk]))

(defn boot
  []
  (clerk/serve!
   {:browse? true
    :watch-paths ["src/aoc/clerk"]}))

(comment
  (boot))
