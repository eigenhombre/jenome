(ns jenome.decode-spec
  (:require [clojure.java.io :refer [as-file resource]]
            [jenome.decode :refer :all]
            [jenome.util :refer [monotonic?]]
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


(describe "yeast metadata"
  (it "decodes correctly"
    (let [hdrs(->> (resource "sacCer3.2bit")
                   as-file
                   sequence-headers)]
      (should= (count hdrs) 17)
      (should (monotonic? (map :dna-offset hdrs)))
      (should= (map :mask-block-count hdrs) (repeat 17 0))
      (should= (map :n-block-sizes hdrs) (repeat 17 ())))))


;; FIXME: overly specific, low-level test?
(describe "determination of read blocks"
  (it "..."
    (should= (get-buffer-starts-and-lengths 0 1000 1000) [[0 1000]])
    (should= (get-buffer-starts-and-lengths 0 1000 1001) [[0 1000] [1000 1]])
    (should= (get-buffer-starts-and-lengths 100 200 512) [[100 200] [300 200]
                                                          [500 112]])))


(describe "is-in-an-n-block"
  (it "determines n-blocks correctly"
    (let [n-block-starts [0 1000]
          n-block-lengths [3 1]]
      (should-not (is-in-an-n-block   -1 n-block-starts n-block-lengths))
      (should     (is-in-an-n-block    0 n-block-starts n-block-lengths))
      (should     (is-in-an-n-block    1 n-block-starts n-block-lengths))
      (should     (is-in-an-n-block    2 n-block-starts n-block-lengths))
      (should-not (is-in-an-n-block    3 n-block-starts n-block-lengths))
      (should-not (is-in-an-n-block  999 n-block-starts n-block-lengths))
      (should     (is-in-an-n-block 1000 n-block-starts n-block-lengths))
      (should-not (is-in-an-n-block 1001 n-block-starts n-block-lengths)))))
