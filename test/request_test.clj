(ns request-test
  (:require [clojure.test :refer :all]
            [babashka.fs :as fs]
            [ring.mock.request :as mock]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [reports-api.core :refer [app]]))

(defn build-mock [endpoint data]
  (assert (map? data) (str "Data must be a map, got" (pr-str data)))

  (-> (mock/request :post endpoint)
      (mock/json-body (json/generate-string data))))

(defn run-test-case [endpoint {:keys [label request-data request-data-path expected-output] :as spec}]
  (assert label (str "Test must have a label, got " (pr-str spec)))
  (assert (or request-data request-data-path)
          (str "Test must have either request-data or request-data-path, got " (pr-str spec)))

  (println "~ Spec" (pr-str spec))

  (testing label
    (let [request-data (if request-data
                         request-data
                         (json/parse-string (slurp request-data-path)))

          response (app (build-mock endpoint request-data))]
      (prn :rd request-data)
      (println "~ Request payload" (pr-str request-data))
      ;; (prn :resp response)
      ))

  ;; (testing label
  ;;   (let [request-data (json/parse-string (slurp request-data-path))
  ;;         response (app (mock/request :post "/api/v1/reports/staff-plan"
  ;;                                     :content-type "application/json"
  ;;                                     :body (json/generate-string request-data)))
  ;;         actual-status (:status response)
  ;;         actual-body (-> response :body (json/parse-string true))]
  ;;     (is (= (:status expected-output) actual-status)
  ;;         (str "Expected status " (:status expected-output)
  ;;              " but got " actual-status))
  ;;     (is (= (:body expected-output) actual-body)
  ;;         (str "Expected response body " (:body expected-output)
  ;;              " but got " actual-body))))
  )

(defn test-endpoint [{:keys [endpoint requests] :as spec}]
  (assert endpoint (str "Test must have an endpoint, got " (pr-str spec)))
  (assert (vector? requests)
          (str "Test must have an array of requests, got " (pr-str spec)))

  (println "➔" endpoint)
  (doseq [r requests] (run-test-case endpoint r)))

(deftest test-requests
  (doseq [file (fs/glob "data/specs" "*.edn")]
    (let [path (str file)]
      (print (str "~ Running " path " "))
      (test-endpoint (edn/read-string (slurp path))))))
