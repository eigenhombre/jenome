(defproject jenome "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :main jenome.core
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [expectations "1.3.3"]
                 [gloss "0.2.2-alpha2"]
                 [incanter/jfreechart "1.0.13-no-gnujaxp"]])
