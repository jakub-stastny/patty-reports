(ns request-test
  (:require [clojure.test :refer :all]
            [babashka.fs :as fs]
            [ring.mock.request :as mock]
            [cheshire.core :as json]
            [reports-api.core :refer [app]]))

;; This is filled in the customer runner.
(def ^:dynamic *test-data* [])

(defn build-mock [endpoint data]
  (assert (map? data) (str "Data must be a map, got" (pr-str data)))

  (-> (mock/request :post endpoint)
      (mock/json-body data)))

(defn run-test-case [endpoint {:keys [label request-data request-data-path expected-output] :as spec}]
  (assert label (str "Test must have a label, got " (pr-str spec)))
  (assert (or request-data request-data-path)
          (str "Test must have either request-data or request-data-path, got " (pr-str spec)))

  (println (str "\n\033[1;32m~ " (:label spec) ":\033[0m"))

  (testing label
    (let [request-data (if request-data
                         request-data
                         (json/parse-string (slurp request-data-path)))

          _ (println "\033[0;36m➔ POST" endpoint "\033[0m")
          _ (println " " (pr-str request-data))

          {:keys [status headers body] :as response} (app (build-mock endpoint request-data))

          data (json/parse-string body true)]
      (println "\n\033[0;33m← HTTP" status "\033[0m")
      (println " " (pr-str data))

      (is (= (:status expected-output) (:status response))
          (str "\033[0;31mExpected status\033[0m " (:status expected-output) ", \033[0;31mbut got\033[0m " status))

      (is (= (:body expected-output) data)
          (str "\033[0;31mExpected body\033[0m " (pr-str (:body expected-output)) ", \033[0;31mbut got\033[0m " data)))))

(defn test-endpoint [{:keys [endpoint requests] :as spec}]
  (assert endpoint (str "Test must have an endpoint, got " (pr-str spec)))
  (assert (vector? requests)
          (str "Test must have an array of requests, got " (pr-str spec)))

  (doseq [r requests] (run-test-case endpoint r)))

(deftest test-requests
  (doseq [spec *test-data*]
    (test-endpoint spec)))
