(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

;; REVENUE ROWS
;; units-sold, sales-revenue-{domestic,eu,rest-of-world}, total-sales-revenue, expected-returns-refunds, net-total-sales-revenue, vat-out-on-net-total-sales-revenue, cost-of-sales, bad-debt-provision, vat-in-on-cost-of-sales, gross-profit
(def revenue-keys [:non-seasonal-revenue-target])

(defn calculate-non-seasonal-revenue-target [inputs results]
  (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
        {:keys [existing-customers sales-growth-rate pro-rata-factor]} results
        price 1]
    (* existing-customers units-per-transaction
       (/ billing-cycles-per-year 12)
       price sales-growth-rate pro-rata-factor)))

(defn revenue-rows [prev-months month inputs results]
  {:non-seasonal-revenue-target (calculate-non-seasonal-revenue-target inputs results)})

;; SALES REVENUE ROWS
;; sales-revenue-due, bad-debts, sales-revenue-received, cost-of-sales-paid, net-cash-flow
(def sales-revenue-keys [])

(defn sales-revenue-rows [prev-months month inputs results]
  {})
