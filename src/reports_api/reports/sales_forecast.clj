(ns reports-api.reports.sales-forecast
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.reports.sales-forecast.validators :as v]
            [reports-api.xhelpers :as xh]
            [reports-api.pro-rata-engine :as pr]
            [reports-api.bubble :as b]
            [reports-api.totals :as tot]))

;; Bound to sales, not to customer number.
(defn month-adjustment-ratios [{:keys [customer-activity-pattern]}]
  (let [base-value (/ 1.0 12)] ; The value for even distribution
    (mapv #(/ % base-value) customer-activity-pattern)))

(defn helper-rows [{:keys [yoy-growth-rate sales-start-date sales-end-date] :as inputs} prev-months month]
  (let [year-index (int (/ (count prev-months) 12))
        sales-growth-rate (nth yoy-growth-rate year-index)
        seasonal-adjustment-rate (nth (month-adjustment-ratios inputs) (dec (:month month)))
        pro-rata-factor (pr/pro-rata-factor (pr/calculate-pro-rata-factor month sales-start-date sales-end-date))]
    {:sales-growth-rate sales-growth-rate :seasonal-adjustment-rate seasonal-adjustment-rate :pro-rata-factor pro-rata-factor}))

;; Generic monthly decay rate.
(defn calculate-monthly-loss-rate-purchase [{:keys [transactions-per-year]}]
  (Math/pow (- 1 (/ transactions-per-year 100.0)) (/ 1.0 12.0)))

(defn calculate-monthly-loss-rate-subscription [{:keys [retention-rate billing-cycles-per-year]}]
  (Math/pow (- 1 (/ retention-rate 100.0)) (/ 1.0 billing-cycles-per-year)))

;; TODO: defmulti?
(def monthly-loss-rate-fns
  {:purchase calculate-monthly-loss-rate-purchase
   :subscription calculate-monthly-loss-rate-subscription})

;; CUSTOMER ROWS
(defn calculate-lost-customers [{:keys [revenue-model] :as inputs} existing-customers pro-rata-factor]
  (let [monthly-loss-rate
        (if-let [calc-fn (get monthly-loss-rate-fns revenue-model)]
          (calc-fn inputs)
          (throw (ex-info "Unsupported revenue model" {:revenue-model revenue-model})))]
    (* existing-customers monthly-loss-rate pro-rata-factor)))

(def customer-keys [:existing-customers :new-customers :lost-customers :total-customers])

(defn customer-rows [{:keys [starting-customers] :as inputs} {:keys [sales-growth-rate pro-rata-factor]} last-month]
  (let [existing-customers (:total-customers (or last-month {:total-customers starting-customers}))
        new-customers (* existing-customers (/ sales-growth-rate 12) pro-rata-factor)
        lost-customers (calculate-lost-customers inputs existing-customers pro-rata-factor)
        total-customers (- (+ existing-customers new-customers) lost-customers)]
    {:existing-customers existing-customers :new-customers new-customers
     :lost-customers lost-customers :total-customers total-customers}))

;; REVENUE ROWS
;; units-sold, sales-revenue-{domestic,eu,rest-of-world}, total-sales-revenue, expected-returns-refunds, net-total-sales-revenue, vat-out-on-net-total-sales-revenue, cost-of-sales, bad-debt-provision, vat-in-on-cost-of-sales, gross-profit
(def revenue-keys [])

(defn revenue-rows [{:keys [] :as inputs} {:keys [] :as results}]
  {})

;; SALES REVENUE ROWS
;; sales-revenue-due, bad-debts, sales-revenue-received, cost-of-sales-paid, net-cash-flow
(def sales-revenue-keys [])

(defn sales-revenue-rows [{:keys [] :as inputs} {:keys [] :as results}]
  {})

(defn generate-report-month [prev-months month inputs]
  (as-> (helper-rows inputs prev-months month) results
    (merge results (customer-rows inputs results (last prev-months)))
    (merge results (revenue-rows inputs results))
    (merge results (sales-revenue-rows inputs results))))

(def tkeys (concat customer-keys revenue-keys sales-revenue-keys))
(def xkeys (conj tkeys :sales-growth-rate :seasonal-adjustment-rate))

(defn handle [raw-inputs]
  (let [inputs (v/validate-inputs raw-inputs)
        _ (prn :inputs inputs)
        projections (xh/generate-projections inputs generate-report-month)]
    (println) (prn :projections projections)
    (tot/add-yearly-totals-one
     (b/format-for-bubble-one projections xkeys)
     tkeys)))
