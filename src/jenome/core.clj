(ns jenome.core
  "
  Decode genome (human or otherwise) in 2-bit format as documented at:
  http://genome.ucsc.edu/FAQ/FAQformat#format7
  "
  (:gen-class)
  (:require [clojure.math.numeric-tower :refer [ceil]]
            [clojure.java.io :refer [resource as-file]]
            [jenome.rafile :refer [read-with-offset]]))


(defn deltas [s]
  (map - (rest s) s))


(defn monotonic?
  "
  Is s monotonically increasing (no decreases)?
  "
  [s]
  (empty? (filter #(< % 0) (deltas s))))


(defn rounding-up-divide [num denom]
  (ceil (/ num denom)))


(defn nybs-to-bases [n]
  (case n
    0 :T
    1 :C
    2 :A
    3 :G
    :default (throw (Exception. "Invalid nybble value!"))))


(defn byte-to-base-pairs [b]
  (let [shifts (range 6 -2 -2)
        nybs (map #(bit-and (bit-shift-right b %) 0x03) shifts)]
    (map nybs-to-bases nybs)))


(defn bytes-to-number [s]
  (->> s
       (map #(bit-and % 0xFF)) ; ...convert negative bytes to "unsigned"
       reverse                 ; ...handle most significant bytes first
                               ; shift and OR them together:
       (reduce (fn [a b] (bit-or (bit-shift-left a 8) b)))))


(defn file-header [fname]
  (let [[sig ver seqcnt resvd] (->> (read-with-offset fname 0 16)
                                    (partition 4)
                                    (map bytes-to-number))]
    (assert (= sig 0x1A412743))
    (assert (= ver 0))
    (assert (= resvd 0))
    seqcnt))


(defn get32 [fname ofs]
  (bytes-to-number (read-with-offset fname ofs 4)))


(defn file-index [fname seqcnt]
  (loop [i 0
         ofs 16
         ret []]
    (if (< i seqcnt)
      (let [[nlen] (read-with-offset fname ofs 1)
            name (apply str (map char (read-with-offset fname (+ ofs 1) nlen)))
            seq-offset (get32 fname (+ ofs 1 nlen))]
        (recur (inc i) (+ ofs nlen 5) (conj ret [nlen name seq-offset])))
      ret)))


(defn skip [offset n] (+ offset (* 4 n)))


(defn getblk
  "
  Get a block of n 32-bit unsigned ints from fname starting at offset.
  n can be 1 (in which case a collection of one element is returned)
  "
  [fname offset n]
  (let [ret (map #(get32 fname (+ offset (* 4 %))) (range n))
        offset (skip offset n)]
    [ret offset]))


(defn sequence-headers
  "
  Get sequence headers from .2bit file, as documented in
  http://genome.ucsc.edu/FAQ/FAQformat#format7. Returns a list of maps
  with details for each sequence.
  "
  [fname]
  (let [seqcnt (file-header fname)]
    (for [[nlen name ofs] (file-index fname seqcnt)]
      (let [[[dna-size]         ofs] (getblk fname ofs 1)
            [[n-block-count]    ofs] (getblk fname ofs 1)
            [n-block-starts     ofs] (getblk fname ofs n-block-count)
            [n-block-sizes      ofs] (getblk fname ofs n-block-count)
            [[mask-block-count] ofs] (getblk fname ofs 1)
            [mask-block-starts  ofs] (getblk fname ofs mask-block-count)
            [mask-block-sizes   ofs] (getblk fname ofs mask-block-count)
            [[reserved]         ofs] (getblk fname ofs 1)]
        (assert (zero? reserved))
        {:name name
         :nlen nlen
         :dna-size dna-size
         :n-block-count n-block-count
         :n-block-starts n-block-starts
         :n-block-sizes n-block-sizes
         :mask-block-count mask-block-count
         ; :mask-block-starts mask-block-starts  ... these are massive for the Human Genome
         ; :mask-block-sizes mask-block-sizes    ...
         :dna-offset ofs}))))


(defn get-buffer-starts-and-lengths 
  "
  Return buffer offsets (starting at ofs) required to cleanly read a
  total of m bytes no more than n at a time
  "
  [ofs n m]
  (loop [a ofs
         len n
         ret []]
    (if (>= a (+ m ofs))
      ret
      (recur (+ a n)
             n
             (conj ret [a (min len (- (+ m ofs) a))])))))


(defn is-in-an-n-block
  ([x hdr]
     (let [{:keys [n-block-starts n-block-sizes]} hdr]
       (is-in-an-n-block x n-block-starts n-block-sizes)))
  ([x n-block-starts n-block-lengths]
     (let [pairs (map (fn [a b] [a (+ a b)])
                      n-block-starts n-block-lengths)]
       (some (fn [[a b]] (< (dec a) x b)) pairs))))


(defn lazy-mapcat
  "
  Fully lazy version of mapcat.  See:
  http://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
  "
  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (lazy-mapcat f (rest coll))))))


(defn genome-sequence
  "
  Read a specific sequence, or all sequences in a file concatenated
  together; return it as a lazy seq.
  "
  ([fname]
     (let [sh (sequence-headers fname)]
       (lazy-mapcat (partial genome-sequence fname) sh)))
  ([fname hdr]
     (let [ofs (:dna-offset hdr)
           dna-len (:dna-size hdr)
           byte-len (rounding-up-divide dna-len 4)
           starts-and-lengths (get-buffer-starts-and-lengths ofs
                                                             10000
                                                             byte-len)]
       (->> (for [[offset length] starts-and-lengths
                  b (read-with-offset fname offset length)]
              (byte-to-base-pairs b))
            (apply concat)
            (take dna-len)
            (map-indexed (fn [i x] (if (is-in-an-n-block i hdr)
                                    :N
                                    x)))))))


(defn genome-str
  "
  Convert e.g. :A :G :T :C to \"AGTC\"
  "
  [s]
  (->> s
       (map name)
       (apply str)))



(defn -main [& args]
  (let [[filename & extra] args]
    (cond
     (seq extra) (println "unknown extra argument(s)" extra)
     (nil? filename) (println "expected file name argument "
                              "(2bit genome format)")
     :else
     (doseq [[ofs dna-len name] (map (juxt :dna-offset :dna-size :name)
                                     (sequence-headers filename))]
       (println (format "%s ----- %d %d" name ofs dna-len))
       (doseq [l (->> (genome-sequence filename)
                          (partition-all 60)
                          (map genome-str))]
         (println l))))))
