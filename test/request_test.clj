(println "XXX")
(ns request-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [reports-api.core :refer [app]]))

(defn load-edn [path]
  (with-open [rdr (io/reader path)]
    (edn/read rdr)))

(defn load-json [path]
  (with-open [rdr (io/reader path)]
    (json/parse-stream rdr true)))

(defn run-test-case [{:keys [label request-data-path expected-output]}]
  (testing label
    (let [request-data (load-json request-data-path)
          response (app (mock/request :post "/api/v1/reports/staff-plan"
                                      :content-type "application/json"
                                      :body (json/generate-string request-data)))
          actual-status (:status response)
          actual-body (-> response :body (json/parse-string true))]
      (is (= (:status expected-output) actual-status)
          (str "Expected status " (:status expected-output)
               " but got " actual-status))
      (is (= (:body expected-output) actual-body)
          (str "Expected response body " (:body expected-output)
               " but got " actual-body)))))

(deftest test-staff-plan-endpoint
  (let [test-specs (load-edn "tests/data/staff_plan.edn")]
    (doseq [test-case test-specs]
      (run-test-case test-case))))
