(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

;; REVENUE ROWS
;; units-sold, sales-revenue-{domestic,eu,rest-of-world}, total-sales-revenue, expected-returns-refunds, net-total-sales-revenue, vat-out-on-net-total-sales-revenue, cost-of-sales, bad-debt-provision, vat-in-on-cost-of-sales, gross-profit
(def subscription-keys [:non-seasonal-revenue-target :required-customers])
(def purchase-keys [:customer-base])
(def revenue-keys (concat subscription-keys purchase-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: subscription.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calculate the base revenue target before applying seasonality.
(defn calculate-non-seasonal-revenue-target [inputs results]
  (h/when-model inputs :subscription
              (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
                    {:keys [existing-customers sales-growth-rate pro-rata-factor price]} results]
                (* existing-customers units-per-transaction
                   (/ billing-cycles-per-year 12)
                   price sales-growth-rate pro-rata-factor))))

;; Once we have our revenue target, we work backwards to figure out how many customers we need.
(defn calculate-required-customers [inputs results]
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
(defn calculate-customer-base [inputs results]
  (h/when-model inputs :purchase
              (let [{:keys [customer-activity-pattern]} inputs
                    {:keys [existing-customers sales-growth-rate pro-rata-factor]} results]
                (* existing-customers sales-growth-rate customer-activity-pattern pro-rata-factor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: either purchase or subscription  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn revenue-rows [prev-months month inputs results]
  ;; TODO: Price
  (as-> (merge {:price 1} results) results
    (assoc results :non-seasonal-revenue-target (calculate-non-seasonal-revenue-target inputs results))
    (assoc results :required-customers (calculate-required-customers inputs results))

    (assoc results :customer-base (calculate-customer-base inputs results))))

;; SALES REVENUE ROWS
;; sales-revenue-due, bad-debts, sales-revenue-received, cost-of-sales-paid, net-cash-flow
(def sales-revenue-keys [:units-sold])

;; Calculate actual units sold based on active customers.
(defn calculate-units-sold [inputs results]
  (let [{:keys [units-per-transaction billing-cycles-per-year]} inputs
        {:keys [active-customers pro-rata-factor]} results
        x (h/if-subscription inputs (/ billing-cycles-per-year 12) 1)]
    (* active-customers units-per-transaction x pro-rata-factor)))

(defn sales-revenue-rows [prev-months month inputs results]
  (as-> (merge {:active-customers 1} results) results
   (assoc results :units-sold (calculate-units-sold inputs results))))
