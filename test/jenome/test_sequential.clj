(ns jenome.test-sequential
  (:use [midje.sweet]
        [clojure.java.io :only [as-file resource]]
        [jenome.sequential]))


(facts "about partition-buffer-sizes"
       (partition-buffer-sizes 100 100)   => [100]
       (partition-buffer-sizes 200 100)   => [100 100]
       (partition-buffer-sizes 201 100)   => [100 100 1])


(facts "about showing numbers with commas"
       (str-with-commas 12345678) => "12,345,678")
