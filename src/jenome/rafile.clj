(ns jenome.rafile
  (:import (java.io RandomAccessFile)))


(defn read-with-offset [fname offset len]
  (let [raf (RandomAccessFile. fname "r")
        bb (byte-array len)]
    (.seek raf offset)
    (.readFully raf bb)
    (.close raf)
    bb))
