(ns reports-api.reports.sales-forecast
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.reports.sales-forecast.validators :as v]
            [reports-api.reports.sales-forecast.helper-rows :as hr]
            [reports-api.reports.sales-forecast.customer-rows :as cr]
            [reports-api.reports.sales-forecast.revenue-rows :as rr]
            [reports-api.reports.sales-forecast.vat-rows :as vr]
            [reports-api.xhelpers :as xh]
            [reports-api.bubble :as b]
            [reports-api.totals :as tot]))

;; TODO: Review all of these and get rid of the multi-key merge (rather than assoc) properties.
(def ^:private row-namespaces
  ['reports-api.reports.sales-forecast.helper-rows
   'reports-api.reports.sales-forecast.revenue-rows
   'reports-api.reports.sales-forecast.customer-rows
   'reports-api.reports.sales-forecast.vat-rows])

;; (defn generate-report-month [prev-months month inputs]
;;   (as-> (helper-rows prev-months month inputs) results
;;     (merge results (cr/customer-rows prev-months month inputs results (last prev-months)))
;;     (merge results (rr/revenue-rows prev-months month inputs results))
;;     (merge results (rr/sales-revenue-rows prev-months month inputs results))))
(defn generate-report-month [prev-months month inputs]
  (let [processing-fns (keep #(ns-resolve % 'process) row-namespaces)]
    (reduce (fn [results f]
              (merge results (f prev-months month inputs results)))
            {} processing-fns)))

(def row-ns-props
  (reduce (fn [acc namespace]
            (let [all-fns (map str (keys (ns-publics namespace)))

                  calculate-fns
                  (filter #(str/starts-with? % "calculate-") all-fns)

                  properties
                  (map #(str/replace-first % #"^calculate-" "") calculate-fns)]
              (merge {(keyword namespace) (map keyword properties)})))
          {}
          row-namespaces))

(def tkeys (apply concat (vals row-ns-props)))
(def xkeys (conj tkeys :sales-growth-rate :seasonal-adjustment-rate))

(defn handle [raw-inputs]
  (let [inputs (v/validate-inputs raw-inputs)
        _ (prn :inputs inputs)
        projections (xh/generate-projections inputs generate-report-month)]
    (println) (prn :projections projections)
    (tot/add-yearly-totals-one
     (b/format-for-bubble-one projections xkeys)
     tkeys)))
