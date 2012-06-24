(ns jenome.core
  (:use expectations)
  (:require [gloss.core :as gl]
            [gloss.io :as glio]
            [clojure.java.io :as io]))

(gl/defcodec hdr-codec [:uint32-le :uint32-le :uint32-le :uint32-le])

(gl/defcodec byte-codec [:byte])

(gl/defcodec index-codec [:byte
                          (gl/repeated [:byte] :prefix :byte)
                          :uint32-le])

(defn get-bytes [n inf]
  (let [buf (byte-array n)]
    (.read inf buf)
    buf))

(let [infile (io/input-stream "/Users/jacobsen/Desktop/hg19.2bit")
      [signature version sequence-count reserved] (glio/decode hdr-codec (get-bytes 16 infile))]
  (assert (= signature 0x1A412743))
  (assert (= version reserved 0))
  (let [[name-size] (glio/decode byte-codec (get-bytes 1 infile))]
    (println "name-size" name-size)
    (println sequence-count "sequences in file.")))
