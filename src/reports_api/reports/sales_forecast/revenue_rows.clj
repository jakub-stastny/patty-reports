(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

;; REVENUE ROWS
;; units-sold, sales-revenue-{domestic,eu,rest-of-world}, total-sales-revenue, expected-returns-refunds, net-total-sales-revenue, vat-out-on-net-total-sales-revenue, cost-of-sales, bad-debt-provision, vat-in-on-cost-of-sales, gross-profit
;; (def subscription-keys [:non-seasonal-revenue-target :required-customers])
;; (def purchase-keys [:customer-base])
;; (def revenue-keys (concat subscription-keys purchase-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: subscription.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calculate the base revenue target before applying seasonality, using initial customer base.
(h/calculate :non-seasonal-revenue-target
             (h/when-model inputs :subscription
                           (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
                                 {:keys [existing-customers sales-growth-rate pro-rata-factor price]} results]
                             (* existing-customers units-per-transaction
                                (/ billing-cycles-per-year 12)
                                price sales-growth-rate pro-rata-factor))))

;; Once we have our revenue target, we work backwards to figure out how many customers we need.
(h/calculate :required-customers
             (h/when-model inputs :subscription
                           (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
                                 {:keys [non-seasonal-revenue price pro-rata-factor price]} results]
                             (if (= 0 pro-rata-factor)
                               0
                               (/ non-seasonal-revenue price units-per-transaction (/ billing-cycles-per-year 12))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: purchase.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calculate base customer numbers for one-time purchases.
(h/calculate :customer-base
             (h/when-model inputs :purchase
                           (let [{:keys [customer-activity-pattern]} inputs
                                 {:keys [existing-customers sales-growth-rate pro-rata-factor]} results]
                             (* existing-customers sales-growth-rate customer-activity-pattern pro-rata-factor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: either.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(h/calculate :domestic-sales
             (let [{:keys [relative]} month
                   {:keys [domestic-sales]} inputs
                   {:keys [total-revenue]} results
                   relative-year (h/get! relative :year)]
               (h/assertions fn-name total-revenue [number?] "Total revenue must be a number")
               (* total-revenue (h/get! domestic-sales relative-year))))

(h/calculate :eu-sales
             (let [{:keys [relative]} month
                   {:keys [eu-sales]} inputs
                   {:keys [total-revenue]} results
                   relative-year (h/get! relative :year)]
               (h/assertions fn-name total-revenue [number?] "Total revenue must be a number")
               (* total-revenue (h/get! eu-sales relative-year))))

(h/calculate :rest-of-world-sales
             (let [{:keys [relative]} month
                   {:keys [rest-of-world-sales]} inputs
                   {:keys [total-revenue]} results
                   relative-year (h/get! relative :year)]
               (h/assertions fn-name total-revenue [number?] "Total revenue must be a number")
               (* total-revenue (h/get! rest-of-world-sales relative-year))))

(h/calculate :returns-and-refunds
             (let [{:keys [refund-returns-allowance]} inputs
                   {:keys [total-revenue]} results]
               (h/assertions fn-name total-revenue [number?] "Total revenue must be a number")
               (* -1 total-revenue refund-returns-allowance)))

(h/calculate :net-sales-revenue
  (let [{:keys [total-revenue returns-and-refunds]} results]
    (h/assertions fn-name total-revenue [number?] "Total revenue must be a number")
    (- total-revenue returns-and-refunds)))

;; SALES REVENUE ROWS
;; sales-revenue-due, bad-debts, sales-revenue-received, cost-of-sales-paid, net-cash-flow
;; (def sales-revenue-keys [:units-sold])

;; Calculate actual units sold based on active customers.
(h/calculate :units-sold
  (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
        {:keys [active-customers pro-rata-factor]} results
        x (h/if-subscription inputs (/ billing-cycles-per-year 12) 1)]
    (* active-customers units-per-transaction x pro-rata-factor)))

(defn process [prev-months month inputs results]
  (h/calculate-properties
   'reports-api.reports.sales-forecast.revenue-rows
   [:non-seasonal-revenue-target :required-customers :customer-base :domestic-sales
    :eu-sales :rest-of-world-sales :returns-and-refunds :net-sales-revenue :units-sold]
   (merge results {:price 1 :total-revenue 10000 :active-customers 1}) prev-months month inputs))
