(ns jenome.core
  (:use expectations)
  (:require [gloss.core :as gl]
            [gloss.io :as glio]
            [clojure.math.numeric-tower :as math]
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

(defn decode-sequence-fields
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
    (assert (= reserved 0))
    {:dna-size dna-size
     :n-block-count n-block-count
     :mask-block-count mask-block-count}))

(defn nybs-to-bases [n]
  (case n
    0 :T
    1 :C
    2 :A
    3 :G
    :default (throw (Exception. "Invalid nybble value!"))))

(defn byte-to-base-pair [b]
  (let [shifts (range 6 -2 -2)
        nybs (map #(bit-and (bit-shift-right b %) 0x03) shifts)]
    (map nybs-to-bases nybs)))

(defn rounding-up-divide [num denom]
  (math/ceil (/ num denom)))

(defn partition-buffers
  "Return buffer sizes required to cleanly read a total of n bytes no more than m at a time"
  [n m]
  (let [remainder (mod n m)]
    (if (zero? remainder)
      (repeat (/ n m) m)
      (concat (repeat (dec (/ n m)) m) [remainder]))))

(defn hg
  ([]
     (hg 1000000))
  ([blocksiz]
     (let [infile (io/input-stream @genome-file)
           seqcount (sequence-count infile)
           index (doall (for [i (range seqcount)]
                          (get-seq-header infile)))
           seqnames (map first index)]
       (doseq [seqn (range seqcount)]
         (println "Decoding sequence" seqn)
         (let [header (decode-sequence-fields infile)
               dna-size (header :dna-size)
               dna-bytes (rounding-up-divide dna-size 4)
               read-sizes (partition-buffers dna-bytes blocksiz)]
           (println header)
           (doseq [bufsiz read-sizes]
             (doall
              (mapcat byte-to-base-pair (get-bytes bufsiz infile)))
             (print ".")
             (flush)))
         (println)))))

(print (time (hg 100000)))
