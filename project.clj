(defproject extract-struct "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main extract-struct.core
  :profiles {:uberjar {:aot :all}}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.reader "0.8.13"]
                 [org.clojure/core.match "0.3.0-alpha4"]])
