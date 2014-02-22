(defproject lein-instability "0.0.1"
  :description "A plugin to generate a namespace dependency chart and calcuate namespace instability."
  :dependencies [[org.clojure/clojure         "1.5.1"]
                 [org.clojure/tools.namespace "0.2.4"]]
  :eval-in-leiningen true

  :profiles {:dev {:dependencies [[speclj "2.5.0"]]}}
  :plugins [[speclj "2.5.0"]]
  :test-paths ["spec"]
)
