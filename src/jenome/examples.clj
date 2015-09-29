(ns jenome.examples
  (:require [jenome.core :refer :all]
            [jenome.graphs :refer :all]
            [clojure.java.io :refer [resource as-file]]))


; Adjust location to suit:
(def human "/Users/jacobsen/Programming/Lisp/Clojure/jenome/hg19.2bit")
(def yeast (as-file (resource "sacCer3.2bit")))
(def yeast-ascii (as-file (resource "sacCer3.fasta")))


(defmacro tib
  "
  tib: Time in the Background
  Run body in background, printing body and showing result when it's done.
  "
  [expr]
  `(future (let [code# '~expr
                 start# (. System (nanoTime))]
             (println "Starting" code#)
             (let [result# ~expr
                   end# (. System (nanoTime))
                   dursec# (/ (double (- end# start#)) (* 1000 1000 1000.0))]
               (println (format "Code: %s\nTime: %.6f seconds\nResult: %s"
                                code#
                                dursec#
                                result#))
               result#))))

(defn count'
  "
  Non-overflowing version of count
  "
  [s]
  (loop [s s, n 0] (if (seq s)
                     (recur (rest s)
                            (inc' n))
                     n)))


(defn write-seq
  "
  Write a (potentially very long) sequence of lines to a text file
  "
  [filename s]
  (with-open [wrt (clojure.java.io/writer filename)]
    (doseq [x s]
      (.write wrt (str x "\n")))))


(defmacro pseq
  "
  Apply threading of funcs in parallel through all sequences specified
  in the index of fname.
  "
  [fname & funcs]
  `(pmap #(->> (genome-sequence ~fname %)
               ~@funcs)
         (sequence-headers ~fname)))


(defn randgenome
    "
    For testing, in case we need a random 'null-hypothesis' genome:
    "
    []
    (repeatedly #(rand-nth [:A :G :C :T])))


(def aminos (apply hash-map '(    UUU F      CUU L      AUU I      GUU V
                                  UUC F      CUC L      AUC I      GUC V
                                  UUA L      CUA L      AUA I      GUA V
                                  UUG L      CUG L      AUG M      GUG V
                                  UCU S      CCU P      ACU T      GCU A
                                  UCC S      CCC P      ACC T      GCC A
                                  UCA S      CCA P      ACA T      GCA A
                                  UCG S      CCG P      ACG T      GCG A
                                  UAU Y      CAU H      AAU N      GAU D
                                  UAC Y      CAC H      AAC N      GAC D
                                  UAA Stop   CAA Q      AAA K      GAA E
                                  UAG Stop   CAG Q      AAG K      GAG E
                                  UGU C      CGU R      AGU S      GGU G
                                  UGC C      CGC R      AGC S      GGC G
                                  UGA Stop   CGA R      AGA R      GGA G
                                  UGG W      CGG R      AGG R      GGG G)))


(defn triplet-to-aminos
  [three]
  (->> three
       (map name)
       (apply str)
       symbol))


(defn is-amino-translating-sequence
  ""
  [sym]
  (not (nil? (aminos sym))))


(comment

  (->> yeast
       genome-sequence
       (map #(if (= % :T) :U %))
       (drop 1)
       (partition 3)
       (take 3000)

       (map triplet-to-aminos)
       (map is-amino-translating-sequence)
       (partition-by identity)
       (map count)
       ;; (make-hist 0.5 60.5 60)
       ;; trim-zeros
       ;; (draw-hist "Encoding lengths, yeast")
       )

  (monotonic? (map :dna-offset (sequence-headers human))) ;;=> true

  ;; Get the names of the sequences (mostly chromosomes)
  (map :name (sequence-headers yeast))

  ;; Get the first 100 base pairs of yeast:
  (->> yeast
       genome-sequence
       (take 100)
       genome-str)  

  ;; How many base pairs?
  (tib (->> yeast
            genome-sequence
            count'))

  ;; Do it in parallel:
  (tib (apply + (pseq yeast count')))
  
  ;; How many base pairs?
  (tib (apply + (pseq human (fn [s] (->> s
                                        (take (* 1000))
                                        count')))))

  
  ;; Relative frequencies of base pairs:
  (tib (->> yeast
            genome-sequence
            frequencies))

  
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

  (tib
   (let [h2 (second (sequence-headers human))
         {:keys [name dna-offset dna-size]} h2]
     (let [fname (str "/tmp/decoded/" name ".fa")]
       (write-seq fname
                  (cons (str ">" name)
                        (->> (genome-sequence human h2)
                             (partition-all 50)
                             (map genome-str)))))))


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


  (->> yeast
       genome-sequence
       frequencies)

  (tib (->> yeast
            genome-sequence
            (partition-by identity)
            (map count)
            frequencies
            (into (sorted-map))))

  
  (tib (apply (partial merge-with +)
              (pseq yeast
                    (partition-by identity)
                    (map count)
                    frequencies
                    (into (sorted-map)))))

  (tib (apply (partial merge-with +)
              (pseq human
                    (remove #{:N})
                    (partition-by identity)
                    (map count)
                    frequencies
                    (into (sorted-map)))))
;; Code: (apply (partial merge-with +) (pseq human (remove #{:N})
;; (partition-by identity) (map count) frequencies (into
;; (sorted-map))))
;; Time: 17218.424726 seconds
;; Result: {1 1457848027, 2 379139089, 3 126579350, 4 42237362, 5
;; 14613038, 6 4215486, 7 1617976, 8 574575, 9 326215, 10 211234, 11
;; 128797, 12 100108, 13 87741, 14 77897, 15 69379, 16 59442, 17
;; 47857, 18 38422, 19 32555, 20 27627, 21 24402, 22 22362, 23 19628,
;; 24 16927, 25 13982, 26 10854, 27 8469, 28 6814, 29 5223, 30 3815,
;; 31 2442, 32 1735, 33 1319, 34 974, 35 873, 36 868, 37 898, 38 806,
;; 39 680, 40 561, 41 389, 42 274, 43 200, 44 165, 45 148, 46 147, 47
;; 132, 48 105, 49 80, 50 63, 51 61, 52 46, 53 30, 54 30, 55 22, 56
;; 19, 57 10, 58 11, 59 4, 60 5, 61 1, 62 2, 63 2, 65 1, 66 1, 68 2,
;; 69 1, 70 1, 72 1, 73 1, 79 1, 81 1, 83 1, 90 1}

  (defn combine-two-hists [a b]
    (map (fn [[x1 y1] [x2 y2]] [x1 (+ y1 y2)]) a b))

  (defn combine-hists [nbins seq-of-hists]
    (reduce combine-two-hists (for [i (range nbins)] [i 0]) seq-of-hists))
  
  (doseq [f [#(take 1000000 (randgenome))
             #(genome-sequence yeast)
             #(genome-sequence human)]]
    (tib
     (->> (f)
          (partition 100000)
          (map get-lengths)
          (pmap #(make-hist 0.5 60.5 60 %))
          (map trim-zeros)
          (combine-hists 60)
          (draw-hist "Repeat Lengths"))))

  (tib
   (->> (randgenome)
        (take 1000000)
        get-lengths
        (make-hist 0.5 60.5 60)
        trim-zeros
        (draw-hist "Repeat Lengths, random hypothesis")))

  (tib
   (->> (genome-sequence yeast)
        get-lengths
        (make-hist 0.5 60.5 60)
        trim-zeros
        (draw-hist "Repeat Lengths, S. Cerviciae")))

  (tib
   (->> (genome-sequence-ascii yeast-ascii)
        get-lengths
        (make-hist 0.5 60.5 60)
        trim-zeros
        (draw-hist "Repeat Lengths, S. Cerviciae")))

  (tib
   (->> (genome-sequence human)
        (take 10000000)
        get-lengths
        (make-hist 0.5 60.5 60)
        trim-zeros
        (draw-hist "Repeat Lengths, human genome")))


  (defn lazy-file-lines
    "
    http://stackoverflow.com/questions/4118123/
    read-a-very-large-text-file-into-a-list-in-clojure/13312151#13312151
    "
    [file]
    (letfn [(helper [rdr]
              (lazy-seq
               (if-let [line (.readLine rdr)]
                 (cons line (helper rdr))
                 (do (.close rdr) nil))))]
      (helper (clojure.java.io/reader file))))

  (defn genome-sequence-ascii [f]
    (->> (lazy-file-lines f)
         (remove #(re-find #"^chr" %))
         (apply concat)
         (map (comp keyword str))))

  (with-out-str
    (time ))
  (->> yeast-ascii
       genome-sequence-ascii
       (drop 160000000)
       (take 10))
  
  (->> yeast-ascii
        genome-sequence-ascii
        (take 100000)
        seque
        get-lengths
        (make-hist 0.5 60.5 60)
        trim-zeros
        (draw-hist "ASCII"))
  

  ;; Get frequencies for all sequences in parallel
  (tib
   (let [fname human
         hdrs (sequence-headers fname)
         freq-fn (fn [hdr] (->> (genome-sequence fname hdr)
                               (take 100000)
                               frequencies))]
     (apply (partial merge-with +) (pmap freq-fn hdrs))))


  
  (def m {:N 239850802, :T 856055361, :A 854963149, :C 592966724, :G 593325228})
  (float  (/ (:N m) (apply + (vals m))))

  
  (tib (->> yeast
            genome-sequence
            frequencies))

  (tib (apply (partial merge-with +)
              (pseq yeast frequencies)))


  ;; Problems with count:
  (tib (count (range (* 1000 1000 1000 3))))


  ;; Dictionary words
  ;;  (def wordfile "/usr/share/dict/words")
  (def wordfile "/tmp/words.csv")
  (require '[clojure.string :as str])
  (def wordlines (str/split (slurp wordfile) #"\n"))

  (def words
    (->> wordlines
         (map #(str/split % #","))
         (map first)))

  ;; convert to lowercase
  (defn tolower [s] (apply str (map #(Character/toLowerCase %) s)))

  (tolower "AAABADddcc")

  (def lcwords (map tolower words))

  (defn is-only-basepairs [s] (every? (set "agct") s))

  (def only-basepair-words (filter is-only-basepairs lcwords))

  (group-by count only-basepair-words)

  )

