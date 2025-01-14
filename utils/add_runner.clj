(ns add-runner
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [babashka.http-client :as http]
            [cheshire.core :as json]))

(defn post [endpoint payload]
  (http/post (str "http://localhost:8080" endpoint)
             {:body payload :throw false}))

(defn read-from-stdin [state label key]
  (print label ": ")
  (flush)
  (let [input (read-line)]
    (assoc state key input)))

(defn format-edn [data]
  (with-out-str (pprint data)))

(defn save-edn [path data]
  (spit path (format-edn data)))

(defn -main [path]
  (let [data (edn/read-string (slurp path))
        new-request (-> {}
                        (read-from-stdin "Label" :label)
                        (read-from-stdin "Request data path" :request-data-path))]
    (save-edn path (merge data {:requests (into requests [new-request])}))
    (println "The following request was added:")
    (println (str "\n  " (pr-str new-request)))
    (println "Don't forget to retrieve its response by running:")
    (println "\n  clojure -M:refresh" path)))
