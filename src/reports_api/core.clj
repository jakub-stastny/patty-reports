(ns reports-api.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json]
            [ring.adapter.jetty :refer [run-jetty]]
            [reports-api.helpers :as h]
            [reports-api.reports.staff-plan :refer [handle] :rename {handle handle-staff-plan}])
  (:gen-class))

(defn format-json [object]
  ;; (json/generate-string object {:pretty true})
  (json/generate-string object))

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
    (let [raw-inputs (parse-json-body request)
          inputs (h/transform-keys-to-kebab-case raw-inputs)
          _ (prn :provided-inputs inputs)
          result (handle-fn inputs)
          _ (prn :response result)]
      (response 200 (h/transform-keys-to-snake-case result)))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (if (= :validation-error (:type data))
          (response 400 data)
          (response 500 data))))
    (catch Throwable e
      (prn e)
      (response 500 {:error (str (type e) ": " (.getMessage e))}))))

(defroutes app
  (GET "/api/v1/ping" [] (response 200 {:status "OK" :response "pong"}))
  (POST "/api/v1/reports/staff-plan" request (handler handle-staff-plan request))
  (route/not-found (response 404 {:error "Not found"})))

(defn -main []
  (println "Starting server on port 8080 ...")
  (run-jetty app {:port 8080 :join? false}))
