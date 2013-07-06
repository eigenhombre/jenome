(ns jenome.test-rafile
  (:use [midje.sweet]
        [jenome.rafile]
        [clojure.java.io :only [as-file output-stream delete-file]]))


(defn rollover-byte-array [siz]
  (let [ba (byte-array siz)]
    (doseq [i (range siz)]
      (aset-byte ba i (byte (rem i 128))))
    ba))


(defn make-tmp-binary-file
  [file-path siz]
  (with-open [outfile (output-stream file-path)]
      (.write outfile (rollover-byte-array siz))))


(defmacro with-tmp-binary-file [fname siz & body]
  `(do
     (make-tmp-binary-file ~fname ~siz)
     (let [result# (do ~@body)]
       (delete-file ~fname)
       result#)))


(fact "with-tmp-binary-file works"
      (let [fname "/tmp/adflkhadf"
            f (as-file fname)
            siz 120389]
        (with-tmp-binary-file fname siz
          (.exists f) => true
          (.length f) => siz)
        (.exists f) => false))


(fact "can read a file with random access, getting the right stuff out"
      (let [fname "/tmp/sadklhjasdflkhs"]
        (with-tmp-binary-file fname (+ 12800 128)
          (into [] (read-with-offset fname 12800 128))
          => (range 128))))
