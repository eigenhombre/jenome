(ns jenome.util-spec
  (:require [speclj.core :refer :all]
            [jenome.util :refer :all]))


(describe 'monotonic?
  (it "distinguishes monotonically-increasing sequences"
    (should (monotonic? (range 10)))
    (should-not (monotonic? [0 0 1 0]))
    (should (monotonic? []))))


(describe 'rounding-up-divide
  (it "rounds and divides correctly"
    (should= 1 (rounding-up-divide 4 5))
    (should= 1 (rounding-up-divide 5 5))
    (should= 2 (rounding-up-divide 6 5))
    (should= 2 (rounding-up-divide 10 5))
    (should= 3 (rounding-up-divide 11 5))))
