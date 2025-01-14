(ns refresh-runner
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [cheshire.core :as json]))

(defn post [endpoint payload]
  (http/post (str "http://localhost:8080" endpoint)
             {:body payload
              :content-type :json
              :accept :json}))

(defn format-edn [data]
  (with-out-str (pprint data)))

(defn save-edn [path data]
  (spit path (format-edn data)))

(defn refresh-requests [{:keys [endpoint requests]}]
  (mapv (fn [{:keys [request-data request-data-path] :as request}]
          (merge request {:expected-output {:status status :body body}})
          (if request-data
            (post endpoint (json/generate-string request-data))
            (post endpoint (slurp request-data-path))))
        requests))

(defn -main [path]
  (let [data (edn/read-string (slurp path))]
    (save-edn path (merge data {:requests (refresh-requests data)}))))
