(ns reports-api.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json]
            [ring.adapter.jetty :refer [run-jetty]]
            [reports-api.reports.staff-plan :refer [handle-staff-plan]])
  (:gen-class))

(defn format-json [object]
  (json/generate-string object {:pretty true}))

(defn parse-json-body [request]
  "Parses the JSON body of the request into a Clojure map."
  (let [body (:body request)]
    (json/parse-stream (clojure.java.io/reader body) true)))

(defn response [status body]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (format-json body)})

(defroutes app
  (GET "/api/v1/ping" [] (response 200 {:status "OK"}))
  (POST "/api/v1/reports/staff-plan" request (response 200 (format-json (handle-staff-plan (parse-json-body request)))))
  (route/not-found (response 404 {:error "Not found"})))

;; Entry point
(defn -main []
  (println "Starting server on port 8080...")
  (run-jetty app {:port 8080 :join? false}))
