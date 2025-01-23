(ns reports-api.reports.sales-forecast.customer-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

;; Generic monthly decay rate.
(defn calculate-monthly-loss-rate-purchase [{:keys [transactions-per-year]}]
  (Math/pow (- 1 (/ transactions-per-year 100.0)) (/ 1.0 12.0)))

(defn calculate-monthly-loss-rate-subscription [{:keys [retention-rate billing-cycles-per-year]}]
  (Math/pow (- 1 (/ retention-rate 100.0)) (/ 1.0 billing-cycles-per-year)))

(def monthly-loss-rate-fns
  {:purchase calculate-monthly-loss-rate-purchase
   :subscription calculate-monthly-loss-rate-subscription})

(defn calculate-lost-customers [{:keys [revenue-model] :as inputs} existing-customers pro-rata-factor]
  (let [monthly-loss-rate
        (if-let [calc-fn (get monthly-loss-rate-fns revenue-model)]
          (calc-fn inputs)
          (throw (ex-info "Unsupported revenue model" {:revenue-model revenue-model})))]
    (* existing-customers monthly-loss-rate pro-rata-factor)))

(def customer-keys [:existing-customers :new-customers :lost-customers :total-customers])

(defn customer-rows [prev-months month {:keys [starting-customers] :as inputs} {:keys [sales-growth-rate pro-rata-factor]} last-month]
  (let [existing-customers (:total-customers (or last-month {:total-customers starting-customers}))
        new-customers (* existing-customers (/ sales-growth-rate 12) pro-rata-factor)
        lost-customers (calculate-lost-customers inputs existing-customers pro-rata-factor)
        total-customers (- (+ existing-customers new-customers) lost-customers)]
    {:existing-customers existing-customers :new-customers new-customers
     :lost-customers lost-customers :total-customers total-customers}))
