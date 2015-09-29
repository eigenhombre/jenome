(ns jenome.decode-spec
  (:require [clojure.java.io :refer [as-file resource]]
            [jenome.decode :refer :all]
            [speclj.core :refer :all]))


(describe "utility functions"
  (it "should translate numbers and hex fields to symbolic base pairs"
    (should= [:T :C :A :G] (map nybs-to-bases [0 1 2 3]))
    (should= [:T :C :A :G] (byte-to-base-pairs 0x1B))))


(defn ^:private yeast-section [from to]
  (->> (resource "sacCer3.2bit")
       as-file
       genome-sequence
       (drop from)
       (take (- to from 1))
       (map name)
       (apply str)))


(describe "decoding of example yeast file"
  (it "can be found"
    (should (.exists (as-file (resource "sacCer3.2bit")))))
  (it "should decode correctly at the beginning"
    (->> (resource "sacCer3.2bit")
         as-file
         genome-sequence
         (take 10)
         (map name)
         (apply str)
         (should= "CCACACCACA")))
  (it "should decode correctly at various places"
    (should= (yeast-section 0 11) "CCACACCACA")
    (should= (yeast-section 1 12) "CACACCACAC")
    (should= (yeast-section 10000 10011) "GAATGAATCG")
    (should= (yeast-section 100000 100011) "GGTATTATTT")))
