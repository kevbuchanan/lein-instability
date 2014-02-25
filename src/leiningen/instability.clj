(ns leiningen.instability
  (:require [clojure.java.io               :refer [file]]
            [clojure.pprint                :refer [print-table]]
            [clojure.tools.namespace.parse :refer [deps-from-ns-decl]]
            [clojure.tools.namespace.find  :refer [find-ns-decls-in-dir]]
            [clojure.tools.namespace.dependency :refer [graph depend nodes
                                                        immediate-dependencies
                                                        transitive-dependencies
                                                        immediate-dependents
                                                        transitive-dependents]]))

(defn- instability-score [ce ca]
  (float
    (try
      (/ (count ce) (+ (count ca) (count ce)))
      (catch Exception e 1))))

(def ^:private abstractions #{"defprotocol" "defmulti" "gen-interface"})

(defn- make-file [ns-decl]
  (-> (str "src/" ns-decl)
      (clojure.string/replace "." "/")
      (clojure.string/replace "-" "_")
      (str ".clj")))

(defn- find-abstractions [ns-decl]
  (let [decl-as-file (make-file ns-decl)
        code (try (slurp decl-as-file) (catch Exception e ""))]
    (some #(re-find (re-pattern %) code) abstractions)))

(defn- flag-for-re-examine [instability abstractions]
  (if (and
        (empty? abstractions)
        (< instability 0.3))
    "X"
    ""))

(defn- filter-ns-decls [ns-syms library]
  (filter #(re-find (re-pattern library) (str %)) ns-syms))

(defn- add-to-graph [graph ns-form ns-string]
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

(defn- filter-maybe [ns-decls config]
  (if (:external config)
    ns-decls
    (filter-ns-decls ns-decls (:lib-name config))))

(defn- wrap-filter [finder]
  (fn
    ([graph config]
      (-> (finder graph)
          (filter-maybe config)))
    ([graph node config]
      (-> (finder graph node)
          (filter-maybe config)))))

(def ^:private get-transitive-dependencies
  (-> transitive-dependencies
      wrap-filter))

(def ^:private get-immediate-dependencies
  (-> immediate-dependencies
      wrap-filter))

(def ^:private get-transitive-dependents
  (-> transitive-dependents
      wrap-filter))

(def ^:private get-immediate-dependents
  (-> immediate-dependents
      wrap-filter))

(def ^:private get-nodes
  (-> nodes
      wrap-filter))

(defn- find-common-deps [graph node config]
  (let [users (immediate-dependents graph node)
        common (reduce
                  (fn [all user]
                    (apply conj all (get-immediate-dependencies graph user config)))
                  []
                  users)]
    (reduce
      (fn [tally other-node]
        (assoc tally (str other-node) [(count (filter #{other-node} common)) (count users)]))
      {}
      (clojure.set/difference (set common) #{node}))))

(defn- format-most-common [dep]
  (when dep
    (let [name (first dep)
          numer (first (last dep))
          denom (last (last dep))]
      (str name ", " numer "/" denom))))

(defn- most-common-deps [graph node config]
  (->> (find-common-deps graph node config)
       (sort-by #(/ (first (val %)) (last (val %))))
       reverse
       (map format-most-common)))

(defn- get-dependencies [graph node config]
  (if (:transitive config)
    (get-transitive-dependencies graph node config)
    (get-immediate-dependencies graph node config)))

(defn- get-dependents [graph node config]
  (if (:transitive config)
    (get-transitive-dependents graph node config)
    (get-immediate-dependents graph node config)))

(defn- node-table-attributes [graph node config]
  (let [dependencies (get-dependencies graph node config)
        dependents (get-dependents graph node config)
        instability (instability-score dependencies dependents)
        abstractions (find-abstractions node)]
    {:namespace (str node)
     :dependency-count (count dependencies)
     :dependent-count (count dependents)
     :instability (format "%.1f" instability)
     :abstract-maybe? (if (empty? abstractions) "" "yes")
     :re-examine (flag-for-re-examine instability abstractions)}))

(defn generate-deps-table [graph config]
  (->> (get-nodes graph config)
       (map #(node-table-attributes graph % config))
       (sort-by :instability)))

(defn- reuse-table-attributes [graph node config]
  (let [most-common-deps (most-common-deps graph node config)]
    {:namespace (str node)
     :1 (first most-common-deps)
     :2 (second most-common-deps)
     :3 (nth most-common-deps 2 "")}))

(defn generate-reuse-table [graph config]
  (->> (get-nodes graph config)
       (map #(reuse-table-attributes graph % config))))

(defn- node-tree [graph node config]
  (reduce
    (fn [tree node]
      (conj tree (node-tree graph node config)))
    [(str node)]
    (if (:dependents config)
      (get-immediate-dependents graph node config)
      (get-immediate-dependencies graph node config))))

(defn- format-subtree [subtree prefix]
  (if (empty? subtree)
    ""
    (apply str prefix (format "[%s]" (first subtree)) "\n"
           (map #(format-subtree %1 (str "->" prefix)) (rest subtree)))))

(defn- format-tree [tree]
  (map #(format-subtree % "") tree))

(defn generate-deps-tree [graph config]
  (->> (get-nodes graph config)
       (map #(node-tree graph % config))
       format-tree
       (clojure.string/join "\n")))

(defn- get-config [project args]
  (reduce
    (fn [config key]
      (assoc config key true))
    {:lib-name (:group project)}
    args))

(def ^:private options
  "OPTIONS
  :table - namespece instability table, add :transitive to count transitive dependencies\n
  :tree - dependency tree, add :dependents for dependent tree\n
  :external - include external library namespaces\n")

(defn instability [project & args]
  (if (seq args)
    (let [args (map read-string args)
          config (get-config project args)
          graph (create-graph "src")]
      (when (:table config)
        (print-table (generate-deps-table graph config))
        (println ""))
      (when (:tree config)
        (println (generate-deps-tree graph config))
        (println ""))
      (when (:reuse config)
        (print-table (generate-reuse-table graph config))
        (println "")))
    (println options)))

