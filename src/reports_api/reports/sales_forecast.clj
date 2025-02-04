(ns reports-api.reports.sales-forecast
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.reports.sales-forecast.validators :as v]
            [reports-api.reports.sales-forecast.customer-rows :as cr]
            [reports-api.reports.sales-forecast.revenue-rows :as rr]
            [reports-api.reports.sales-forecast.vat-rows :as vr]
            [reports-api.xhelpers :as xh]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.bubble :as b]
            [reports-api.totals :as tot]))

;; Bound to sales, not to customer number.
(defn month-adjustment-ratios [{:keys [customer-activity-pattern]}]
  (let [base-value (/ 1.0 12)] ; The value for even distribution
    (mapv #(/ % base-value) customer-activity-pattern)))

(defn helper-rows [prev-months month {:keys [yoy-growth-rate sales-start-date sales-end-date] :as inputs}]
  (let [year-index (int (/ (count prev-months) 12))
        sales-growth-rate (nth yoy-growth-rate year-index)
        seasonal-adjustment-rate (nth (month-adjustment-ratios inputs) (dec (:month month)))
        pro-rata-factor (pr/pro-rata-factor (pr/calculate-pro-rata-factor month sales-start-date sales-end-date))]
    {:sales-growth-rate sales-growth-rate :seasonal-adjustment-rate seasonal-adjustment-rate :pro-rata-factor pro-rata-factor}))

;; TODO: Review all of these and get rid of the multi-key merge (rather than assoc) properties.
(def ^:private row-namespaces
  ['reports-api.reports.sales-forecast.vat-rows
   'reports-api.reports.sales-forecast.revenue-rows
   'reports-api.reports.sales-forecast.customer-rows])

;; (defn generate-report-month [prev-months month inputs]
;;   (as-> (helper-rows prev-months month inputs) results
;;     (merge results (cr/customer-rows prev-months month inputs results (last prev-months)))
;;     (merge results (rr/revenue-rows prev-months month inputs results))
;;     (merge results (rr/sales-revenue-rows prev-months month inputs results))))
(defn generate-report-month [prev-months month inputs]
  (let [processing-fns (keep #(ns-resolve % 'rows) row-namespaces)]  ;; Resolve `rows` from each namespace
    (reduce (fn [results f]
              (merge results (f prev-months month inputs)))
            (helper-rows prev-months month inputs)
            processing-fns)))

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
