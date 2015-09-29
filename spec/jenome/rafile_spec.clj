(ns jenome.rafile-spec
  (:require [speclj.core :refer :all]
            [jenome.rafile :refer [read-with-offset]]
            [clojure.java.io :refer [as-file output-stream delete-file]]))


(defn ^:private rollover-byte-array [siz]
  (let [ba (byte-array siz)]
    (doseq [i (range siz)]
      (aset-byte ba i (byte (rem i 128))))
    ba))


(defn ^:private make-tmp-binary-file
  [file-path siz]
  (with-open [outfile (output-stream file-path)]
      (.write outfile (rollover-byte-array siz))))


(defmacro with-tmp-binary-file [fname siz & body]
  `(do
     (make-tmp-binary-file ~fname ~siz)
     (let [result# (do ~@body)]
       (delete-file ~fname)
       result#)))


(describe 'with-tmp-binary-file
  (with fname "/tmp/somefile")
  (with f (as-file @fname))
  (with size 120389)
  (it "creates a file with the right size, and cleans it up"
    (with-tmp-binary-file @fname @size
      (should (.exists @f))
      (should= @size (.length @f)))
    (should-not (.exists @f))))


(describe 'read-with-offset
  (with fname "/tmp/binfile____")
  (it "reads a file with random access, getting the right stuff out"
    (with-tmp-binary-file @fname (+ 12800 128)
      (should= (into [] (read-with-offset @fname 12800 128))
               (range 128)))))
