(ns reports-api.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json]
            [ring.adapter.jetty :refer [run-jetty]]
            [reports-api.reports.staff-plan :refer [handle] :rename {handle handle-staff-plan}])
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

(defn handler [handle-fn request]
  (try
    (response 200 (handle-fn (parse-json-body request)))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (if (= :validation-error (:type data))
          (response 400 data)
          (response 500 data))))
    (catch Exception e
      (prn e)
      (response 500 {:error (str (type e) ": " (.getMessage e))}))))

(defroutes app
  (GET "/api/v1/ping" [] (response 200 {:status "OK" :response "pong"}))
  (POST "/api/v1/reports/staff-plan" request (handler handle-staff-plan request))
  (route/not-found (response 404 {:error "Not found"})))

(defn -main []
  (println "Starting server on port 8080 ...")
  (run-jetty app {:port 8080 :join? false}))
