(ns test-runner
  (:require [clojure.test :as test]
            [clojure.edn :as edn]
            [babashka.fs :as fs]
            [request-test]))

(defn run-tests [paths]
  (let [test-data (map #(edn/read-string (slurp %)) paths)]
    (binding [request-test/*test-data* test-data]
      (test/run-tests 'request-test))))

(defn -main [& args]
  (run-tests
   (if (empty? args)
     (map str (fs/glob "data/specs" "*.edn"))
     args)))
