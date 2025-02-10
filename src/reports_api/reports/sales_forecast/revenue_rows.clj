(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]))

;; Calculate the base revenue target before applying seasonality, using initial customer base.
(property :non-seasonal-revenue-target
          (when-model :subscription
            (* (:existing-customers rs) (:units-per-transaction in)
               (/ (:billing-cycles-per-year in) 12)
               (:price rs) (:sales-growth-rate rs) (:pro-rata-factor rs))))

;; Once we have our revenue target, we work backwards to figure out how many customers we need.
(property :required-customers
          (when-model :subscription
            (if (= 0 (:pro-rata-factor rs))
              0
              (/ (:non-seasonal-revenue rs)
                 (:price rs)
                 (:units-per-transaction in)
                 (/ (:billing-cycles-per-year in) 12)))))

;; Calculate base customer numbers for one-time purchases.
(property :customer-base
          (when-model :purchase
            (let [month-activity-pattern
                  (get (:customer-activity-pattern in) (dec (:month month)))]
              (* (:existing-customers rs)
                 (:sales-growth-rate rs)
                 month-activity-pattern
                 (:pro-rata-factor rs)))))

(property :domestic-sales
          (* (:total-revenue rs)
             (h/get! (h/get! in :domestic-sales)
                     (get-in month [:relative :year]))))

(property :eu-sales
          (* (:total-revenue rs)
             (h/get! (h/get! in :eu-sales)
                     (get-in month [:relative :year]))))

(property :rest-of-world-sales
          (* (:total-revenue rs)
             (h/get! (h/get! in :rest-of-world-sales)
                     (get-in month [:relative :year]))))

(property :returns-and-refunds
          (* -1 (:total-revenue rs) (:refund-returns-allowance in)))

(property :net-sales-revenue
          (- (:total-revenue rs) (:returns-and-refunds rs)))

;; Calculate actual units sold based on active customers.
(property :units-sold
          (let [x (if-subscription (/ (:billing-cycles-per-year in) 12) 1)]
            (* (:active-customers rs) (:units-per-transaction in) x (:pro-rata-factor rs))))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.revenue-rows
   [:non-seasonal-revenue-target :required-customers :customer-base :domestic-sales
    :eu-sales :rest-of-world-sales :returns-and-refunds :net-sales-revenue :units-sold]

   ;; results
   (merge results {:price 1 :total-revenue 10000 :active-customers 1})

   prev-months month inputs))
