(ns leiningen.instability
  (:require [clojure.java.io               :refer [file]]
            [clojure.pprint                :refer [print-table]]
            [clojure.tools.namespace.parse :refer [deps-from-ns-decl read-ns-decl]]
            [clojure.tools.namespace.file  :refer [read-file-ns-decl]]
            [clojure.tools.namespace.find  :refer [find-namespaces-in-dir find-ns-decls-in-dir]]
            [clojure.tools.namespace.dependency :refer [graph depend nodes
                                                        immediate-dependencies
                                                        transitive-dependencies
                                                        immediate-dependents
                                                        transitive-dependents]]))

(defn instability-score [ce ca]
  (float (/ (count ce) (+ (count ca) (count ce)))))

(defn filter-nodes [graph library]
  (filter #(re-find (re-pattern library) (str %)) (nodes graph)))

(defn add-to-graph [graph ns-form ns-string]
  (reduce
    (fn [new-graph deps]
      (depend new-graph ns-form deps))
    graph
    (deps-from-ns-decl ns-string)))

(defn create-graph [directory]
  (let [ns-strings (find-ns-decls-in-dir (file directory))]
    (reduce
      (fn [graph ns-string]
        (add-to-graph graph (second ns-string) ns-string))
      (graph)
      ns-strings)))

(defn node-attributes [graph node]
  (let [dependencies (transitive-dependencies graph node)
        dependents (transitive-dependents graph node)
        instability (instability-score dependencies dependents)]
    {:namespace (str node)
     :dependency-count (count dependencies)
     :dependent-count (count dependents)
     :instability (format "%.1f" instability)}))

(defn print-graph [graph library]
  (->> (filter-nodes graph library)
       (map #(node-attributes graph %))
       (sort-by :instability)
       print-table))

(defn instability [project & args]
  (let [graph (create-graph "src")]
    (print-graph graph (:name project))))
