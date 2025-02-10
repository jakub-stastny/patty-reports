(ns reports-api.reports.sales-forecast.helper-rows
  "These are intermediate values that are repeatedly used in subsequent calculations."
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.time :as t]))

;; TODO: Apart from checking manually, regenerate the specs.
;; There however we might need to break the things down
;; rather than testing the whole blob.
;; I think the whole blob testing is useful, but these
;; two things should be probably done in parallel.

;; Show actual and relative month for easier inspection.
(property :month (t/format-month month))
(property :relative-month (t/month-to-int (:relative month)))

(property :being-sold (t/<=
                       (t/date-to-month (:sales-start-date in))
                       month
                       (t/date-to-month (:sales-end-date in))))

;; The inputs.yoy-growth-rate vector contains (decimal) rates, where
;; each rate is expected growth per each selling year (not projection year).
;; The rates are not for the first year of selling off the product but for
;; the first year of selling off the product in the projection period.
(property :sales-growth-rate
          (if (:being-sold rs)
            (let [month-diff (- (t/month-to-int month)
                                (t/month-to-int (t/date-to-month (:sales-start-date in))))
                  year-index (int (/ month-diff 12))]
              ;; (prn :1st-month [(:sales-start-date in) first-month])
              ;; (prn :current-month month)
              ;; (prn :yi year-index)

              (nth (:yoy-growth-rate in) year-index))

            0))

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
   [:month :relative-month :being-sold :sales-growth-rate :seasonal-adjustment-rate :pro-rata-factor]
   results prev-months month inputs))
