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

(defn filter-ns-decls [ns-syms library]
  (filter #(re-find (re-pattern library) (str %)) ns-syms))

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

(defn filter-maybe [ns-decls config]
  (if (:external config)
    ns-decls
    (filter-ns-decls ns-decls (:lib-name config))))

(defn wrap-filter [finder]
  (fn
    ([graph config]
      (-> (finder graph)
          (filter-maybe config)))
    ([graph node config]
      (-> (finder graph node)
          (filter-maybe config)))))

(def get-transitive-dependencies
  (-> transitive-dependencies
      wrap-filter))

(def get-immediate-dependencies
  (-> immediate-dependencies
      wrap-filter))

(def get-transitive-dependents
  (-> transitive-dependents
      wrap-filter))

(def get-immediate-dependents
  (-> immediate-dependents
      wrap-filter))

(def get-nodes
  (-> nodes
      wrap-filter))

(defn graph-map [graph node config]
  {:namespace (str node)
   :transitive-dependencies (get-transitive-dependencies graph node config)
   :immediate-dependencies (get-immediate-dependencies graph node config)
   :transitive-dependents (get-transitive-dependents graph node config)
   :immediate-dependents (get-immediate-dependents graph node config)})

(defn create-graph-maps [graph config]
  (reduce
    (fn [maps node]
      (conj maps (graph-map graph node config)))
  []
  (get-nodes graph config)))

(defn node-table-attributes [graph node config]
  (let [dependencies (get-transitive-dependencies graph node config)
        dependents (get-transitive-dependents graph node config)
        instability (instability-score dependencies dependents)]
    {:namespace (str node)
     :dependency-count (count dependencies)
     :dependent-count (count dependents)
     :instability (format "%.1f" instability)}))

(defn print-deps-table [graph config]
  (->> (get-nodes graph config)
       (map #(node-table-attributes graph % config))
       (sort-by :instability)
       print-table))

(defn node-tree [graph node config]
  (reduce
    (fn [tree node]
      (conj tree (node-tree graph node config)))
    [(str node)]
    (get-immediate-dependencies graph node config)))

(defn format-subtree [subtree prefix]
  (if (empty? subtree)
    ""
    (apply str prefix (first subtree) "\n"
           (map #(format-subtree %1 (str "\t" prefix)) (rest subtree)))))

(defn format-tree [tree]
  (map #(format-subtree % "[") tree))

(defn print-deps-tree [graph config]
  (->> (get-nodes graph config)
       (map #(node-tree graph % config))
       format-tree
       (clojure.string/join "\n\n")
       println))

(defn instability [project & args]
  (let [graph (create-graph "src")
        config {:lib-name (:name project)}]
    (print-deps-table graph config)
    (println "\n----------------------\n")
    (print-deps-tree graph config)))

