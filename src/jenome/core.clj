(ns jenome.core
  (:use expectations)
  (:require [gloss.core :as gl]
            [gloss.io :as glio]
            [clojure.java.io :as io]))

"
Decode Human Genome in 2-bit format as documented at:
http://genome.ucsc.edu/FAQ/FAQformat#format7
"

(gl/defcodec hdr-codec (vec (repeat 4 :uint32-le)))
(gl/defcodec byte-codec [:byte])
(gl/defcodec u32 :uint32-le)

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

(defn get32
  [infile]
  (glio/decode u32 (get-bytes 4 infile)))

(defn getwords
  [n infile]
  (glio/decode-all u32 (get-bytes (* n 4) infile)))

(def genome-file
  (atom "/Users/jacobsen/Programming/Lisp/Clojure/jenome/hg19.2bit"))

(defn get-seq-header
  [infile]
  (let [name-len-byte (get-bytes 1 infile)
        name-len (first (glio/decode byte-codec name-len-byte))
        seqname (bytes-to-str (get-bytes name-len infile))
        offset (get32 infile)]
    [seqname offset]))


(defn deltas [s]
  (map - (rest s) s))

(defn monotonic? [s]
  (empty? (filter #(< % 0) (deltas s))))

(defn decode-sequence
  [infile]
  (let [dna-size (get32 infile)
        n-block-count (get32 infile)
        n-block-starts (getwords n-block-count infile)
        n-block-sizes (getwords n-block-count infile)
        mask-block-count (get32 infile)
        mask-block-starts (getwords mask-block-count infile)
        mask-block-sizes (getwords mask-block-count infile)
        reserved (get32 infile)]

    (assert (monotonic? n-block-starts))
    (assert (monotonic? mask-block-starts))

    {:dna-size dna-size
     :n-block-count n-block-count
     :mask-block-count mask-block-count
     :reserved reserved}))

(let [infile (io/input-stream @genome-file)
      seqcount (sequence-count infile)
      index (doall (for [i (range seqcount)]
                     (get-seq-header infile)))
      seqnames (map first index)]
  (println (decode-sequence infile))
  (println (map int (get-bytes 260 infile))))
