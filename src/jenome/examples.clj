(ns jenome.examples
  (:use jenome.core
        [clojure.java.io :only [resource as-file]]))


; Adjust location to suit:
(def human "/Users/jacobsen/Programming/Lisp/Clojure/jenome/hg19.2bit")
(def yeast (as-file (resource "sacCer3.2bit")))


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

(tib (count (range (* 1000 1000 1000 3))))


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


(comment

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
  
  (tib
   (->> (randgenome)
        (take 1000000)
        (partition-by identity)
        (map count)
        frequencies
        (into (sorted-map))))
  
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

  )
