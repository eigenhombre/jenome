(ns jenome.util
  (:require [clojure.math.numeric-tower :refer [ceil]]))


(defn count'
  "
  Non-overflowing version of count
  "
  [s]
  (loop [s s, n 0] (if (seq s)
                     (recur (rest s)
                            (inc' n))
                     n)))


(defn monotonic?
  "
  Is s monotonically increasing (no decreases)?
  "
  [s]
  (->> s
       (partition 2 1)
       (map (partial apply -))
       (filter (partial < 0))
       empty?))


(defn rounding-up-divide [num denom]
  (ceil (/ num denom)))


(defn lazy-mapcat
  "
  Fully lazy version of mapcat.  See:
  http://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
  "
  [f coll]
  (lazy-seq
   (if (seq coll)
     (concat (f (first coll))
             (lazy-mapcat f (rest coll))))))




