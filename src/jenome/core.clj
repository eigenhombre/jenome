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


(defn my-mapcat
  "
  http://clojurian.blogspot.com/2012/11/beware-of-mapcat.html
  "
  [f coll]
  (lazy-seq
   (if (not-empty coll)
     (concat
      (f (first coll))
      (my-mapcat f (rest coll))))))

(defn genome-sequence
  "
  Read a specific sequence, or all sequences in a file concatenated
  together; return it as a lazy seq.
  "
  ([fname]
     (let [sh (sequence-headers fname)
           base-offset (-> sh first :dna-offset)]
       (my-mapcat (partial genome-sequence fname base-offset) sh)))
  ([fname base-offset hdr]
     (let [ofs (:dna-offset hdr)
           dna-len (:dna-size hdr)
           byte-len (rounding-up-divide dna-len 4)
           starts-and-lengths (get-buffer-starts-and-lengths ofs 10000 byte-len)]
       (->> (for [[offset length] starts-and-lengths
                  :let [buf (read-with-offset fname offset length)]
                  b buf]
              (byte-to-base-pairs b))
            (apply concat)
            (take dna-len)
            (map-indexed (fn [i x] (if (is-in-an-n-block (+' i (- ofs base-offset)) hdr)
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
  (def yeast
    (as-file (resource "sacCer3.2bit")))
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
  (defn randgenome []
    (repeatedly #(rand-nth [:A :G :C :T])))
  
  (time (->> yeast
             genome-sequence
             count))
  
  ;; Use pmap and partition-all instead -- uses about 360% of my
  ;; 4-core MBP, same as count by itself
  (->> human
       genome-sequence
       (partition-all 100000)
       (pmap count)
       (apply +))

  (defn count' [s]   ;; Regular count overflows to negative int!
    (loop [s s, n 0] (if (seq s)
                       (recur (rest s)
                              (inc' n))
                       n)))

  (-> yeast
      genome-sequence
      count')
  
  (->> human
       genome-sequence
       (drop 10000)
       (take 100)
       (map name)
       (apply str))
  

  (clojure.pprint/pprint
   (let [seqcnt (file-header yeast)]
     (file-index yeast seqcnt)))

  
  (.mkdir (clojure.java.io/file "/tmp/decoded"))
  
  (let [h1 (first (sequence-headers human))
        h2 (second (sequence-headers human))
        base-offset (:dna-offset h1)
        {:keys [name dna-offset dna-size]} h2]
    (let [fname (str "/tmp/decoded/" name ".fa")]
      (write-seq fname
                 (cons (str ">" name)
                       (->> (genome-sequence human base-offset hdr)
                            (partition-all 50)
                            (map genome-str))))))


  (->>
   human
   genome-sequence
   (take 100)
   frequencies)

  (->> "chr1.fa"
       (str "/Users/jacobsen/Programming/Lisp/Clojure/jenome/")
       clojure.java.io/reader
       line-seq
       (drop 1)
       (apply concat)
       (map str)
       (map #(.toUpperCase %))
       (take 10000)
       frequencies)

  (sequence-headers yeast)
  (sequence-headers human)
  (file-header human)
  (file-header yeast)

  ;; Get all "blank" spots
  (->> human
       sequence-headers
       first
       ((juxt :n-block-starts :n-block-sizes))
       (apply interleave)
       (partition 2)
       (map vec))

  (->>
   human
   genome-sequence
   (map-indexed vector)
   (take 100))

  (->>
   human
   genome-sequence
   (map name)
   (drop 10000)
   (take 1000)
   (apply str))

  (->>
   human
   genome-sequence
   (take 1000)
   (partition-by identity)
   (map (partial map name))
   (map (partial apply str)))
  
  (split-with identity (ran))
  ;; Approach for proceeding:
  ;; require genome-sequence to take an argument which is the entire
  ;; header structure.  have it use that to find the Ns.

  (float (/ 3137161264 12157105))

  (->> human
       sequence-headers
       (map (juxt :name :dna-size :n-block-count))
       clojure.pprint/pprint)

  (->> human
       sequence-headers
       first
       ((juxt :n-block-starts :n-block-sizes))
       (apply interleave)
       (partition 2)
       (map vec)
       clojure.pprint/pprint)

  (->> (range (* 1000 1000 1000 3))
       (partition (* 1000 1000))
       (pmap count)
       (apply +))

  )


