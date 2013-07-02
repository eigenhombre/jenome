(ns jenome.core
  (:gen-class)
  (:use midje.sweet
        [clojure.java.io :only [resource]]
        [gloss.core :only [defcodec repeated]]
        [gloss.io :only [decode]])
  (:require [gloss.core :as gl]
            [gloss.io :as glio]
            [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io]))

"
Decode genome (human or otherwise) in 2-bit format as documented at:
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

(defn get-sequence-count-from-file-header
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

(defn decode-sequence-block-header
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

(defn partition-buffer-sizes
  "Return buffer sizes required to cleanly read a total of n bytes no more than m at a time"
  [n m]
  (let [remainder (mod n m)]
    (if (zero? remainder)
      (repeat (/ n m) m)
      (concat (repeat (dec (/ n m)) m) [remainder]))))


(defn sequence-index [infile seqcount]
  (for [i (range seqcount)]
    (decode-sequence-block-header infile)))


(defn decode-genome
  "
  Make a lazy sequence of base pairs from a given .2bit genome file
  "
  ([fname] (decode-genome 100000 fname))
  ([blocksiz fname]
     (apply concat
             (let [infile (io/input-stream fname)
                   seqcount (get-sequence-count-from-file-header infile)
                   ;; Don't care about the index, but need to read the
                   ;; bytes to get to the right position:
                   _ (doall (sequence-index infile seqcount))]
               (for [i (range seqcount)
                     :let [header (decode-sequence-fields infile)
                           dna-size (:dna-size header)
                           dna-bytes (rounding-up-divide dna-size 4)
                           read-sizes (partition-buffer-sizes dna-bytes blocksiz)]
                     r read-sizes
                     :let [inner (get-bytes r infile)]
                     b inner]
                 (byte-to-base-pair b))))))


(defn str-with-commas [n]
  (->> n
       str
       reverse
       (partition-all 3)
       (interpose \,)
       flatten
       reverse
       (apply str)))


(defn printing-counter
  ([s] (printing-counter 1000N s))
  ([ival s]
     (loop [c 0
            s s]
       (if-not (seq s)
         (println "\nTotal:" (str-with-commas c))
         (do
           (if (zero? (rem c ival)) (print (str "\r" (str-with-commas c))))
           (recur (inc' c) (rest s)))))))


(defn -main [& args]
  (let [[filename & extra] args]
    (cond
     (seq extra) (println "unknown extra argument(s)" extra)
     (nil? filename) (println "expected file name argument (2bit genome format)")
     :else
     (do
       (println (->> (first args)
                     decode-genome
                     (take 1000)
                     (map name)
                     (apply str)))
       (printing-counter (decode-genome (first args)))))))
