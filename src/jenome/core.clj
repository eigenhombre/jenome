(ns jenome.core
  (:use expectations)
  (:require [gloss.core :as gl]
            [gloss.io :as glio]
            [clojure.java.io :as io]))

(gl/defcodec hdr-codec [:uint32-le :uint32-le :uint32-le :uint32-le])

(def buffer (byte-array 16))

(.read (io/input-stream "/Users/jacobsen/Desktop/hg19.2bit") buffer)

(let [[signature version sequence-count reserved] (glio/decode hdr-codec buffer)]
  (assert (= signature 0x1A412743))
  (assert (= version reserved 0))
  (println sequence-count "sequences in file."))
