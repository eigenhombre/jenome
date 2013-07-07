(ns jenome.core
  "
  Decode genome (human or otherwise) in 2-bit format as documented at:
  http://genome.ucsc.edu/FAQ/FAQformat#format7
  "
  (:gen-class)
  (:use midje.sweet
        [clojure.math.numeric-tower :only [ceil]]
        [clojure.java.io :only [resource as-file]]
        [jenome.rafile :only [read-with-offset]]))


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


(defmacro stepthrough [offset bindings & body]
  `(let [ofs# ~offset
         ~@(apply concat (for [[a# aa# b# bb#] (partition 4 bindings)]
                           [a# aa# b# bb#]))] ~@body))


(defn sequence-headers
  "
  Get sequence headers from .2bit file, as documented in
  http://genome.ucsc.edu/FAQ/FAQformat#format7. Returns a list of maps
  with details for each sequence.
  "
  [fname]
  (let [seqcnt (file-header fname)
        index (file-index fname seqcnt)]
    (for [[nlen name offset] (file-index fname seqcnt)]
      (stepthrough offset [dna-size (get32 fname offset)
                           offset (skip offset 1)
                           
                           n-block-count (get32 fname offset)
                           offset (skip offset 1)
                           
                           n-block-starts (map #(get32 fname (+ offset (* 4 %))) (range n-block-count))
                           offset (skip offset n-block-count)
                           
                           n-block-sizes  (map #(get32 fname (+ offset (* 4 %))) (range n-block-count))
                           offset (skip offset n-block-count)
                           
                           mask-block-count (get32 fname offset)
                           offset (skip offset 1)
                           
                           mask-block-starts (map #(get32 fname (+ offset (* 4 %))) (range mask-block-count))
                           offset (skip offset mask-block-count)
                           
                           mask-block-sizes (map #(get32 fname (+ offset (* 4 %))) (range mask-block-count))
                           offset (skip offset mask-block-count)
                           
                           reserved (get32 fname offset)
                           offset (skip offset 1)]
        (assert (zero? reserved))
        {:name name
         :nlen nlen
         :dna-size dna-size
         :n-block-count n-block-count
         :n-block-starts n-block-starts
         :n-block-sizes n-block-sizes
         :mask-block-starts mask-block-starts
         :mask-block-sizes mask-block-sizes
         :dna-offset offset}))))


(defn get-buffer-starts-and-lengths 
  "
  Return buffer offsets (starting at ofs) required to cleanly read a
  total of n bytes no more than m at a time
  "
  [ofs n m]
  (loop [a ofs
         len n
         ret []]
    (if (> a (+ m ofs))
      ret
      (recur (+ a n)
             n
             (conj ret [a (min len (- (+ m ofs) a))])))))


(defn genome-sequence
  "
  Read a specific sequence, or all sequences in a file concatenated
  together; return it as a lazy seq.
  "
  ([fname]
     ;; (apply concat
     ;;        (for [[ofs dna-len] (map (juxt :dna-offset :dna-size) (sequence-headers fname))]
     ;;          (genome-sequence fname ofs dna-len)))
     (let [sh (sequence-headers fname)]
       (mapcat #(genome-sequence fname %1 %2)
               (map :dna-offset sh)
               (map :dna-size sh)))
     )
  ([fname ofs dna-len]
     (take dna-len
           (apply concat
                  (let [byte-len (rounding-up-divide dna-len 4)
                        starts-and-lengths (get-buffer-starts-and-lengths ofs 10000 byte-len)]
                    (for [[offset length] starts-and-lengths
                          :let [buf (read-with-offset fname offset length)]
                          b buf]
                      (byte-to-base-pairs b)))))))


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
     (nil? filename) (println "expected file name argument (2bit genome format)")
     :else
     (doseq [[ofs dna-len name] (map (juxt :dna-offset :dna-size :name)
                                     (sequence-headers filename))]
       (println (format "%s ----- %d %d" name ofs dna-len))
       (doseq [l (->> (genome-sequence filename ofs dna-len)
                          (partition-all 60)
                          (map genome-str))]
         (println l))))))


(comment

  (def human "/Users/jacobsen/Programming/Lisp/Clojure/jenome/hg19.2bit")
  (def yeast "/Users/jacobsen/Programming/Lisp/Clojure/jenome/resources/sacCer3.2bit")
  (clojure.pprint/pprint (sequence-headers human))
  (clojure.pprint/pprint (sequence-headers yeast))
  (monotonic? (map :dna-offset (sequence-headers human))) ;;=> true

  ;; Get the names of the sequences (mostly chromosomes)
  (map :name (sequence-headers yeast))

  ;; Just get the first 100 base pairs:
  (->> yeast
       genome-sequence
       (take 100)
       genome-str)
  
  ;; Simple write of all sequences together in the order they are in the file:

  (defn write-seq 
    "
    Write a (potentially very long) sequence of lines to a text file
    "
    [filename s]
    (with-open [wrt (clojure.java.io/writer filename)]
      (doseq [x s]
        (.write wrt (str x "\n")))))

  (write-seq "/tmp/myyeast"
             (->> yeast
                  genome-sequence
                  (partition-all 60)
                  (map genome-str)))

  ;; Write a file comparable to downloaded FASTA file (use chromosome
  ;; order):
  (defn get-yeast-seqmaps-in-order
    "
    Yeast chromosome sequences in 2-bit file are out of order; this puts
    them sequential by chromosome number.
    "
    [fname]
    (let [seqmaps (sequence-headers fname)]
      (for [seqname (map str '(chrI chrII chrIII chrIV chrV chrVI chrVII chrVIII
                                    chrIX chrX chrXI chrXII chrXIII chrXIV chrXV chrXVI chrM))]
        (first (filter #(= seqname (:name %)) seqmaps)))))
  
  (write-seq "/tmp/myyeast"
             (apply concat
                    (for [[ofs dna-len name] (map (juxt :dna-offset :dna-size :name)
                                                  (get-yeast-seqmaps-in-order yeast))]
                      (cons (format "%s ----- %d %d" name ofs dna-len)
                            (->> (genome-sequence yeast ofs dna-len)
                                 (partition-all 60)
                                 (map genome-str))))))

  ;; How many base pairs?
  (->> yeast
       genome-sequence
       count)

  ;; How many base pairs?
  (with-out-str
    (time
     (->> human
          genome-sequence
          (take (* 1000 1000 1))
          count)))

  
  ;; Relative frequencies of base pairs:
  (->> yeast
       genome-sequence
       frequencies)

  ;; In case we need it, here's an infinite random genome:
  (defn infinite-sim-genome []
    (repeatedly #(rand-nth [:A :G :C :T])))

  ;; A serial counter - uses 140% of my 4-core MBP, then blows heap:
  (->> human
       genome-sequence
       (apply count))

  ;; Use pmap and partition-all instead -- uses about 360% of my 4-core MBP
  (->> human
       genome-sequence
       (partition-all 100000)
       (pmap count)
       (apply +))


  (def yeast
    (as-file (resource "sacCer3.2bit")))

  (clojure.pprint/pprint
   (let [seqcnt (file-header yeast)]
     (file-index yeast seqcnt)))

)


