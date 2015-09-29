(ns jenome.core
  (:gen-class)
  (:require [jenome.decode :refer [sequence-headers
                                   genome-sequence
                                   genome-str]]))


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
