(ns jenome.sequential
  "
  Decode genome (human or otherwise) in 2-bit format as documented at:
  http://genome.ucsc.edu/FAQ/FAQformat#format7

  Sequential implementation, file must be processed end-to-end.
  "
  (:gen-class)
  (:use midje.sweet
        jenome.core
        [clojure.math.numeric-tower :only [ceil]]
        [clojure.java.io :only [resource]]
        [gloss.core :only [defcodec repeated]]
        [gloss.io :only [decode decode-all]]
        [jenome.rafile :only [read-with-offset]]))


(defcodec u32 :uint32-le)
(defcodec hdr-codec (repeat 4 u32))

(defn get-bytes
  [n inf]
  (let [buf (byte-array n)]
    (.read inf buf)
    buf))

(defn get-sequence-count-from-file-header
  [infile]
  (let [hdr-bytes (get-bytes 16 infile)
        [signature ver cnt reserved] (decode hdr-codec hdr-bytes)]
  (assert (= signature 0x1A412743))
  (assert (= ver reserved 0))
  cnt))

(defn bytes-to-str
  [bytes]
  (apply str (map (comp char int) bytes)))

(defn read32
  [infile]
  (decode u32 (get-bytes 4 infile)))

(defn getwords
  [n infile]
  (decode-all u32 (get-bytes (* n 4) infile)))

(defn decode-sequence-block-header
  [infile]
  (let [[name-len & _] (get-bytes 1 infile)
        seqname (bytes-to-str (get-bytes name-len infile))
        offset (read32 infile)]
    [seqname offset]))

(defn decode-sequence-fields
  [infile]
  (let [dna-size (read32 infile)
        n-block-count (read32 infile)
        n-block-starts (getwords n-block-count infile)
        n-block-sizes (getwords n-block-count infile)
        mask-block-count (read32 infile)
        mask-block-starts (getwords mask-block-count infile)
        mask-block-sizes (getwords mask-block-count infile)
        reserved (read32 infile)]

    (assert (monotonic? n-block-starts))
    (assert (monotonic? mask-block-starts))
    (assert (= reserved 0))
    {:dna-size dna-size
     :n-block-count n-block-count
     :mask-block-count mask-block-count}))



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
             (let [infile (clojure.java.io/input-stream fname)
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
                 (byte-to-base-pairs b))))))


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


(defn -main-old [& args]
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
                     (apply str)) "...")
       (printing-counter (decode-genome (first args)))))))