(ns reports-api.core
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(defroutes app
  (GET "/" [] {:status 200
               :headers {"Content-Type" "application/json"}
               :body "{\"message\": \"Welcome to my API!\"}"})
  (GET "/hello" [] {:status 200
                    :headers {"Content-Type" "application/json"}
                    :body "{\"message\": \"Hello, world!\"}"})
  (route/not-found {:status 404
                    :headers {"Content-Type" "application/json"}
                    :body "{\"error\": \"Not Found\"}"}))

;; Entry point
(defn -main []
  (println "Starting server on port 8080...")
  (run-jetty app {:port 8080 :join? false}))
