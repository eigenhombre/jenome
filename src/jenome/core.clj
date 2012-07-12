(ns jenome.core
  (:use expectations)
  (:require [gloss.core :as gl]
            [gloss.io :as glio]
            [clojure.java.io :as io]))

"
Decode Human Genome in 2-bit format as documented at:
http://genome.ucsc.edu/FAQ/FAQformat#format7
"

(gl/defcodec hdr-codec [:uint32-le :uint32-le :uint32-le :uint32-le])
(gl/defcodec byte-codec [:byte])
(gl/defcodec bc2 (gl/repeated :byte) );[:byte :byte :byte :byte])
(gl/defcodec seq-hdr-offset :uint32-le)

(defn get-bytes
  [n inf]
  (let [buf (byte-array n)]
    (.read inf buf)
    buf))

(defn sequence-count
  [infile]
  (let [hdr-bytes (get-bytes 16 infile)
        [signature ver cnt reserved] (glio/decode hdr-codec hdr-bytes)]
  (assert (= signature 0x1A412743))
  (assert (= ver reserved 0))
  cnt))

(defn bytes-to-str
  [bytes]
  (apply str (map (comp char int) bytes)))

(defn get-seq-header
  [infile]
  (let [name-len-byte (get-bytes 1 infile)
        name-len (first (glio/decode byte-codec name-len-byte))
        seqname (bytes-to-str (get-bytes name-len infile))
        offset (glio/decode seq-hdr-offset (get-bytes 4 infile))]
    [seqname offset]))

(let [fname "/Users/jacobsen/Programming/Lisp/Clojure/jenome/hg19.2bit"
      infile (io/input-stream fname)
      seqcount (sequence-count infile)
      index (for [_ (range seqcount)]
              (get-seq-header infile))]
  (apply str (interpose " " (map first index))))
