(defproject jenome "1.0.0-SNAPSHOT"
  :description "Hacking around with the human genome"
  :main jenome.core
  :profiles {:dev {:dependencies [[speclj "3.3.0"]]
                   :plugins [[speclj "3.3.0"]]}}
  :test-paths ["spec"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [incanter/jfreechart "1.0.13-no-gnujaxp"]])
