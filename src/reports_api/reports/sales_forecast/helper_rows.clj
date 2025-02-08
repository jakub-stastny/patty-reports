(ns reports-api.reports.sales-forecast.helper-rows
  "These are intermediate values that are repeatedly used in subsequent calculating"
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.time :as t]))

;; Show actual and relative month for easier inspection.
(property :month (t/format-month month))
(property :relative-month (t/month-to-int (:relative month)))

(property :sales-growth-rate
          (let [year-index (int (/ (count prev-months) 12))]
            (nth (:yoy-growth-rate in) year-index)))

(property :seasonal-adjustment-rate
          (let [base-value (/ 1.0 12) ; The value for even distribution
                month-adjustment-ratios
                (mapv #(/ % base-value) (:customer-activity-pattern in))]
            (nth month-adjustment-ratios (dec (:month month)))))

(property :pro-rata-factor
          (pr/pro-rata-factor
           (pr/calculate-pro-rata-factor month (:sales-start-date in) (:sales-end-date in))))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.helper-rows
   [:month :relative-month :sales-growth-rate :seasonal-adjustment-rate :pro-rata-factor]
   results prev-months month inputs))
