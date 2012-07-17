(ns jenome.test
  (:use [expectations])
  (:use [jenome.core]))

(expect (map nybs-to-bases [0 1 2 3]) [:T :C :A :G])
(expect (byte-to-base-pair 0X1B) [:T :C :A :G])

(expect (rounding-up-divide 4 5) 1)
(expect (rounding-up-divide 5 5) 1)
(expect (rounding-up-divide 6 5) 2)
(expect (rounding-up-divide 10 5) 2)
(expect (rounding-up-divide 11 5) 3)

(expect (partition-buffers 100 100) [100])
(expect (partition-buffers 200 100) [100 100])
(expect (partition-buffers 201 100) [100 100 1])

