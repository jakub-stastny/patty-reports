(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]))

;; Calculate the base revenue target before applying seasonality, using initial customer base.
(property :non-seasonal-revenue-target
          (when-model :subscription
            (* (rs! :existing-customers) (in! :units-per-transaction)
               (/ (in! :billing-cycles-per-year) 12)
               (in! :selling-price) (rs! :sales-growth-rate) (rs! :pro-rata-factor))))

;; Once we have our revenue target, we work backwards to figure out how many customers we need.
(property :required-customers
          (when-model :subscription
            (if (= 0 (rs! :pro-rata-factor))
              0
              (/ (rs! :non-seasonal-revenue)
                 (in! :selling-price)
                 (in! :units-per-transaction)
                 (/ (in! :billing-cycles-per-year) 12)))))

;; Calculate base customer numbers for one-time purchases.
(property :customer-base
          (when-model :purchase
            (let [month-activity-pattern
                  (h/get! (:customer-activity-pattern in) (dec (:month month)))]
              (* (rs! :existing-customers)
                 (rs! :sales-growth-rate)
                 month-activity-pattern
                 (rs! :pro-rata-factor)))))

;; TODO: apply selling price changes.
(property :total-revenue
          0)

(property :domestic-sales
          (* (rs! :total-revenue)
             (h/get! (in! :domestic-sales)
                     (get-in month [:relative :year]))))

(property :eu-sales
          (* (rs! :total-revenue)
             (h/get! (in! :eu-sales)
                     (get-in month [:relative :year]))))

(property :rest-of-world-sales
          (* (rs! :total-revenue)
             (h/get! (in! :rest-of-world-sales)
                     (get-in month [:relative :year]))))

(property :returns-and-refunds
          (* -1 (rs! :total-revenue) (in! :refund-returns-allowance)))

(property :net-sales-revenue
          (- (rs! :total-revenue) (rs! :returns-and-refunds)))

;; Calculate actual units sold based on active customers.
(property :units-sold
          (let [x (if-subscription (/ (in! :billing-cycles-per-year) 12) 1)]
            (* (rs! :active-customers) (in! :units-per-transaction) x (rs! :pro-rata-factor))))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.revenue-rows
   [:non-seasonal-revenue-target :required-customers :customer-base :domestic-sales
    :total-revenue
    :eu-sales :rest-of-world-sales :returns-and-refunds :net-sales-revenue :units-sold]

   ;; results
   (merge results {:active-customers 1})

   prev-months month inputs))
