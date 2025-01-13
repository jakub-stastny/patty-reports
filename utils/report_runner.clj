(ns report-runner
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [cheshire.core :as json]))

(defn url [endpoint]
  (str "http://localhost:8080" endpoint))

(defn post [& chunks]
  (println "\033[1;36m$\033[0m" (str/join " " chunks) "\n")
  (let [{:keys [exit out err] :as results} (apply sh chunks)]
    (prn (json/parse-string out true))))

(defn -main
  ([path] (-main path nil))

  ([path index-str]
   (let [{:keys [endpoint requests]} (edn/read-string (slurp path))
         index (when index-str (Integer/parseInt index-str))
         requests-to-process (if index [(nth requests index)] requests)]
     (doseq [{:keys [request-data request-data-path]} requests-to-process]
       (if request-data
         (let [payload (json/generate-string request-data)]
           (post "curl" (url endpoint) "--json" payload))
         (post "curl" (url endpoint) "--json" (str "@" request-data-path)))))
   (System/exit 0)))
