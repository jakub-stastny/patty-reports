(ns reports-api.reports.sales-forecast.helper-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.time :as t]))

;; This was the original code:
;;
;; (defn helper-rows [prev-months month {:keys [yoy-growth-rate sales-start-date sales-end-date] :as inputs}]
;;   (let [year-index (int (/ (count prev-months) 12))
;;         sales-growth-rate (nth yoy-growth-rate year-index)
;;         seasonal-adjustment-rate (nth (month-adjustment-ratios inputs) (dec (:month month)))]
;;     {:sales-growth-rate sales-growth-rate :seasonal-adjustment-rate seasonal-adjustment-rate :pro-rata-factor pro-rata-factor}))

(h/calculate :sales-growth-rate
             (let [{:keys [yoy-growth-rate]} inputs
                   year-index (int (/ (count prev-months) 12))]
               (nth yoy-growth-rate year-index)))

;; Bound to sales, not to customer number.
(defn- month-adjustment-ratios [{:keys [customer-activity-pattern]}]
  (let [base-value (/ 1.0 12)] ; The value for even distribution
    (mapv #(/ % base-value) customer-activity-pattern)))

(h/calculate :seasonal-adjustment-rate
             (let [{:keys [yoy-growth-rate]} inputs]
               (nth (month-adjustment-ratios inputs) (dec (:month month)))))

(h/calculate :pro-rata-factor
             (let [{:keys [sales-start-date sales-end-date]} inputs]
               (pr/pro-rata-factor (pr/calculate-pro-rata-factor month sales-start-date sales-end-date))))

(defn process [prev-months month inputs results]
  (h/calculate-properties
   'reports-api.reports.sales-forecast.helper-rows
   [:sales-growth-rate :seasonal-adjustment-rate :pro-rata-factor]
   results prev-months month inputs))
