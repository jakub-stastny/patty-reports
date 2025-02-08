(ns reports-api.reports.sales-forecast.helper-rows
  "These are intermediate values that are repeatedly used in subsequent calculating"
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.time :as t]))

;; Show actual and relative month for easier inspection.
(property :month (t/format-month month))
(property :relative-month (t/month-to-int (:relative month))) ; TODO: Should the {:month m :year y :relative {:y y :m m}} be instead in this format?

;; The inputs.yoy-growth-rate vector contains (decimal) rates, where
;; each rate is expected growth per each selling year (not projection year).
(property :sales-growth-rate
          (let [ ;; This is the original approx, using projection years:

                ;;year-index
                ;; (int (/ (count prev-months) 12))

                ;; New approach that uses selling years, but it's unclear how it should handle past data.
                ;; Technically you can fill with 0s to offset until the projection year.
                ;;
                ;; Make sure this is validate if we go with it.

                first-month
                (t/date-to-month (:sales-start-date in))

                month-diff
                (-
                 (t/month-to-int month)
                 (t/month-to-int first-month))

                year-index (/ month-diff 12)]
            (prn :1st-month [(:sales-start-date in) first-month])
            (prn :current-month month)))

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
