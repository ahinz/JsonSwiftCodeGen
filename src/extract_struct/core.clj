(ns extract-struct.core
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.string :as s]
            [clojure.java.io :as io])
  (:gen-class))

;; http://blog.fogus.me/2013/07/17/an-introduction-to-deep-code-walking-macros-with-clojure/
(defn check-for-decl-fn [sym]
  (fn [form]
    (when (seq? form)
      (let [[op & _] form]
        (and
         op (symbol? op) (= op sym))))))

(def struct-decl? (check-for-decl-fn 'struct_decl))
(def var-decl? (check-for-decl-fn 'var_decl))

(defn is-struct? [tree]
  (and
   (struct-decl? tree)
   (contains? (set (partition 2 1 tree)) '(inherits JsonModel))))

(defn strip-single-quotes [s]
  (.replaceAll s "'" ""))

(defn decompose-var [[_ name & other]]
  {"name" name
   "type" (last (last (filter #(= (first %) 'type=) (partition 2 1 other))))})

(defn process-struct [[_ name & rest]]
  {:name name
   :vars (->> rest
              (filter var-decl?)
              (map decompose-var))})

(defn apply-indent [lines]
  (map #(.concat "    " %) lines))

(defn generate-curried-static-fn [name args return body]
  (letfn [(format-argument [[name type]] (str "(" name ": " type ")"))]
    (concat [(str "static func " name
                  (apply str (map format-argument args))
                  " -> " return " {")]
            (apply-indent body)
            ["}"])))

(defn generate-curried-create-fn [swift-type args]
  (let [format-arg (fn [[k _]] (str k ": " k))
        body [(str "return " swift-type "(" (s/join ", " (map format-arg args)) ")")]]
    (generate-curried-static-fn "create" args swift-type body)))


;; If we have an optional type we
;; can just do a result cast instead
(defn handle-type-assoc [[name type]]
  (if (.endsWith type "?")
    (str "typecastResult(d[\"" name "\"])")
    (str "d[\"" name "\"] >>> " type ".decode")))

(defn generate-decode-fn [swift-type args]
  (let [raw-mappings (map handle-type-assoc args)
        mappings (s/split (s/join " <*> \n" raw-mappings) #"\n")
        create-stanza (concat
                        [(str swift-type ".create <^>")]
                        (apply-indent mappings))
        closure-stanza (concat
                        [(str "return _JSONObject(json) >>> { d in")]
                        (apply-indent create-stanza)
                        ["}"])]

    (generate-curried-static-fn "decode" [["json" "JSON"]] (.concat swift-type "?") closure-stanza)))

(defn generate-extension-block [swift-type args]
  (s/join "\n"
          (concat [(str "extension " swift-type ": JSONDecodable {")]
                  (apply-indent (generate-curried-create-fn swift-type args))
                  [""]
                  (apply-indent (generate-decode-fn swift-type args))
                  ["}"])))

(defn struct-to-extensions [{name :name vars :vars}]
  (generate-extension-block name (map (fn [v] [(get v "name")
                                               (get v "type")]) vars)))
(def preamble-file (io/resource
                    "preamble.swift"))

(defn load-sanitized-edn-from-string [edn-as-string]
  (edn/read-string
   (.replaceAll
    (.replaceAll
     (s/join "\n"
             (filter #(not (.contains % "import_decl"))
                     (drop-while #(not (.contains % "source_file"))
                                 (s/split edn-as-string #"\n"))))
     "[:@./]" "")
    "'" "\"")))

;; (def read-it (load-sanitized-edn-from-string
;;               (slurp "/Users/ahinz/src/swift/playground/JsonParserTest/JsonParserTest/test.ast")))

(defn parse-edn-to-swift [parsed-edn]
  (->> parsed-edn
       (tree-seq coll? identity)
       (filter is-struct?)
       (map process-struct)
       (map struct-to-extensions)
       (s/join "\n")
       (str (slurp preamble-file) "\n")))

(defn -main [& args]
  (-> *in*
      slurp
      load-sanitized-edn-from-string
      parse-edn-to-swift
      println))
