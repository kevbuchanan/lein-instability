(ns leiningen.instability-spec
  (require [speclj.core                  :refer :all]
           [clojure.java.io              :refer [file]]
           [clojure.tools.namespace.find :refer [find-ns-decls-in-dir]]
           [leiningen.instability        :refer :all]))

(describe "lein instability"
  (with config {:lib-name "leiningen"})
  (with external-config (assoc @config :external true))

  (context "deps table"
    (it "returns a table of namespaces"
      (let [table (generate-deps-table (create-graph "src") @config)]
        (should (every? :namespace table))
        (should (every? :dependency-count table))
        (should (every? :dependent-count table))
        (should (every? :instability table))
        (should (every? :abstract-maybe? table))))

    (it "calculates the instability score"
      (let [table (generate-deps-table (create-graph "src") @config)]
        (should= "1.0" (:instability (last table)))))

    (it "includes external namespaces when configured"
      (let [table (generate-deps-table (create-graph "src") @external-config)]
        (should= "clojure.tools.namespace.find" (:namespace (first table)))))
  )

  (context "deps tree"
    (it "returns a dependency tree by default"
      (let [tree (generate-deps-tree (create-graph "src") @external-config)]
        (should (re-find #"leiningen.instability\]\n->\[clojure.tools.namespace" tree))))

    (it "returns a dependent tree when configured"
      (let [tree (generate-deps-tree (create-graph "src") (assoc @external-config :dependents true))]
        (should (re-find #"clojure.tools.namespace.find\]\n->\[leiningen.instability" tree))))
  )

  (context "with no args"
    (it "prints the options"
      (should= "OPTIONS\n:table\n:tree\n:external\n:transitive\n"
               (with-out-str
                 (instability {}))))
  )
)
