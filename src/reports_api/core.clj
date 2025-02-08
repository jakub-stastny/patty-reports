(ns reports-api.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json]
            [ring.adapter.jetty :refer [run-jetty]]
            [jakub-stastny.extensions.case :as jsc]
            [reports-api.helpers :as h]
            [reports-api.reports.staff-plan :refer [handle] :rename {handle handle-staff-plan}]
            [reports-api.reports.staff-plans :refer [handle] :rename {handle handle-staff-plans}]
            [reports-api.reports.sales-forecast :refer [handle] :rename {handle handle-sales-forecast}])
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

(defn error-response [status error data]
  (println (str "\nHTTP " status ":\n\n  " (pr-str data) "\n"))
  (when error (prn error))
  (response status data))

(defn handler [handle-fn request]
  (try
    (let [raw-inputs (parse-json-body request)
          inputs (jsc/transform-keys-to-kebab-case raw-inputs)
          ;; _ (prn :provided-inputs inputs)
          result (handle-fn inputs)
          ;; _ (prn :response result)
          ]
      (response 200 (jsc/transform-keys-to-snake-case result)))
    (catch clojure.lang.ExceptionInfo error
      (let [data (ex-data error)]
        (if (= :validation-error (:type data))
          (error-response 400 error (merge {:error (:type data)} (dissoc data :type)))
          (error-response 500 error (merge {:error (ex-message error)} data)))))
    (catch Throwable error
      (error-response 500 nil {:error (str (type error) ": " (.getMessage error))}))))

(defroutes app
  (GET "/api/v1/ping" [] (response 200 {:status "OK" :response "pong"}))
  (POST "/api/v1/reports/staff-plan" request (handler handle-staff-plan request))
  (POST "/api/v1/reports/staff-plans" request (handler handle-staff-plans request))
  (POST "/api/v1/reports/sales-forecast" request (handler handle-sales-forecast request))
  (route/not-found #(error-response 404 nil {:error "Not found"})))

(defn -main []
  (println "Starting server on port 8080 ...")
  (run-jetty app {:port 8080 :join? false}))
