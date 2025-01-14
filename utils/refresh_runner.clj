(ns refresh-runner
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [babashka.http-client :as http]
            [cheshire.core :as json]))

(defn post [endpoint payload]
  (http/post (str "http://localhost:8080" endpoint)
             {:body payload :throw false}))

(defn format-edn [data]
  (with-out-str (pprint data)))

(defn save-edn [path data]
  (spit path (format-edn data)))

(defn refresh-requests [{:keys [endpoint requests]}]
  (mapv (fn [{:keys [label request-data request-data-path] :as request}]
          (let [response
                (if request-data
                  (post endpoint (json/generate-string request-data))
                  (post endpoint (slurp request-data-path)))

                status (:status response)
                body (json/parse-string (:body response) true)]
            (if (= status 500)
              (throw (ex-info "HTTP 500" {:endpoint endpoint :label label}))
              (merge request {:expected-output {:status status :body body}}))))
        requests))

(defn -main [path]
  (let [data (edn/read-string (slurp path))]
    (save-edn path (merge data {:requests (refresh-requests data)}))
    (println "Done. Don't forget to verify the results running:")
    (println "\n  clojure -M:test" path)))
