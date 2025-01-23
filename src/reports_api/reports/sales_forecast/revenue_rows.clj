(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

;; REVENUE ROWS
;; units-sold, sales-revenue-{domestic,eu,rest-of-world}, total-sales-revenue, expected-returns-refunds, net-total-sales-revenue, vat-out-on-net-total-sales-revenue, cost-of-sales, bad-debt-provision, vat-in-on-cost-of-sales, gross-profit
(def revenue-keys [:non-seasonal-revenue-target])

;; Calculate the base revenue target before applying seasonality.
(defn calculate-non-seasonal-revenue-target [{:keys [revenue-model] :as inputs} results]
  (if (= revenue-model :subscription)
    (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
          {:keys [existing-customers sales-growth-rate pro-rata-factor price]} results]
      (* existing-customers units-per-transaction
         (/ billing-cycles-per-year 12)
         price sales-growth-rate pro-rata-factor))

    ;; Return blank for purchase.
    0))

;; Once we have our revenue target, we work backwards to figure out how many customers we need.
(defn calculate-required-customers [{:keys [revenue-model] :as inputs} results]
  (if (= revenue-model :subscription)
    (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
          {:keys [non-seasonal-revenue price pro-rata-factor price]} results]
      (if (= 0 pro-rata-factor)
        0
        (/ non-seasonal-revenue price units-per-transaction (/ billing-cycles-per-year 12))))

    ;; Return blank for purchase.
    0))

(defn revenue-rows [prev-months month inputs results]
  ;; TODO: Price
  (as-> (merge {:price 1} results) results
    (assoc results :non-seasonal-revenue-target (calculate-non-seasonal-revenue-target inputs results))
    (assoc results :required-customers (calculate-required-customers inputs results))))

;; SALES REVENUE ROWS
;; sales-revenue-due, bad-debts, sales-revenue-received, cost-of-sales-paid, net-cash-flow
(def sales-revenue-keys [])

(defn sales-revenue-rows [prev-months month inputs results]
  {})
