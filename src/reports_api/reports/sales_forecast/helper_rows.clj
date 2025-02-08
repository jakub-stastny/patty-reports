(ns reports-api.reports.sales-forecast.helper-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.time :as t]))

;; This was the original code:
;;
;; (defn helper-rows [prev-months month {:keys [yoy-growth-rate sales-start-date sales-end-date] :as inputs}]
;;   (let [year-index (int (/ (count prev-months) 12))
;;         sales-growth-rate (nth yoy-growth-rate year-index)
;;         seasonal-adjustment-rate (nth (month-adjustment-ratios inputs) (dec (:month month)))]
;;     {:sales-growth-rate sales-growth-rate :seasonal-adjustment-rate seasonal-adjustment-rate :pro-rata-factor pro-rata-factor}))

(property :sales-growth-rate
          (let [year-index (int (/ (count prev-months) 12))]
            (nth (:yoy-growth-rate in) year-index)))

;; Bound to sales, not to customer number.
(defn- month-adjustment-ratios [{:keys [customer-activity-pattern]}]
  (let [base-value (/ 1.0 12)] ; The value for even distribution
    (mapv #(/ % base-value) customer-activity-pattern)))

(property :seasonal-adjustment-rate
          (nth (month-adjustment-ratios in) (dec (:month month))))

(property :pro-rata-factor
          (pr/pro-rata-factor
           (pr/calculate-pro-rata-factor month (:sales-start-date in) (:sales-end-date in))))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.helper-rows
   [:sales-growth-rate :seasonal-adjustment-rate :pro-rata-factor]
   results prev-months month inputs))
