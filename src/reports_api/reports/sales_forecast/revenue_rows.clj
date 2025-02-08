(ns reports-api.reports.sales-forecast.revenue-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]))

;; REVENUE ROWS
;; units-sold, sales-revenue-{domestic,eu,rest-of-world}, total-sales-revenue, expected-returns-refunds, net-total-sales-revenue, vat-out-on-net-total-sales-revenue, cost-of-sales, bad-debt-provision, vat-in-on-cost-of-sales, gross-profit
;; (def subscription-keys [:non-seasonal-revenue-target :required-customers])
;; (def purchase-keys [:customer-base])
;; (def revenue-keys (concat subscription-keys purchase-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: subscription.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: purchase.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calculate base customer numbers for one-time purchases.
(property :customer-base
          (when-model :purchase
            (let [month-activity-pattern
                  (get (:customer-activity-pattern in) (dec (:month month)))]
              (* (:existing-customers rs)
                 (:sales-growth-rate rs)
                 month-activity-pattern
                 (:pro-rata-factor rs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revenue model: either.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(property :domestic-sales
          (* (:total-revenue rs)
             (h/get! (:domestic-sales in)
                     (get-in month [:relative :year]))))

(property :eu-sales
          (* (:total-revenue rs)
             (h/get! (:eu-sales in)
                     (get-in month [:relative :year]))))

(property :rest-of-world-sales
          (* (:total-revenue rs)
             (h/get! (:rest-of-world-sales in)
                     (get-in month [:relative :year]))))

;; TODO
;; (doseq [geo [:domestic :eu :rest-of-world]]
;;   (let [geo-sales (keyword (str (name geo) "-sales"))]
;;     (prn :x geo geo-sales)
;;     (property geo-sales
;;               (* (:total-revenue rs)
;;                  (h/get! (get in geo-sales)
;;                          (get-in month [:relative :year]))))))

(property :returns-and-refunds
          (* -1 (:total-revenue rs) (:refund-returns-allowance in)))

(property :net-sales-revenue
          (- (:total-revenue rs) (:returns-and-refunds rs)))

;; SALES REVENUE ROWS
;; sales-revenue-due, bad-debts, sales-revenue-received, cost-of-sales-paid, net-cash-flow
;; (def sales-revenue-keys [:units-sold])

;; Calculate actual units sold based on active customers.
(property :units-sold
          (let [x (if-subscription (/ (:billing-cycles-per-year in) 12) 1)]
            (* (:active-customers rs) (:units-per-transaction in) x (:pro-rata-factor rs))))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.revenue-rows
   [:non-seasonal-revenue-target :required-customers :customer-base :domestic-sales
    :eu-sales :rest-of-world-sales :returns-and-refunds :net-sales-revenue :units-sold]
   (merge results {:price 1 :total-revenue 10000 :active-customers 1}) prev-months month inputs))
