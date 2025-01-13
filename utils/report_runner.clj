(ns report-runner
  (:require [clojure.edn :as edn]))

;; (defn load-endpoint [{:keys [endpoint requests] :as spec}]
;;   (assert endpoint (str "Test must have an endpoint, got " (pr-str spec)))
;;   (assert (vector? requests)
;;           (str "Test must have an array of requests, got " (pr-str spec)))

;;   (doseq [r requests] (run-test-case endpoint r)))


(defn -main
  ([path] (-main path nil))

  ([path index-str]
   (let [{:keys [endpoint requests]} (edn/read-string (slurp path))
         index (when index-str (Integer/parseInt index-str))
         requests-to-process (if index [(nth requests index)] requests)]
     (doseq [{:keys [request-data request-data-path]} requests-to-process]
       (if request-data
         (println (str "curl http://localhost:8080" endpoint " --json '" (pr-str request-data) "'"))
         (println (str "curl http://localhost:8080" endpoint " --json @" request-data-path)))))))
