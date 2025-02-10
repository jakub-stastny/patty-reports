(ns reports-api.reports.sales-forecast.customer-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]))

(property :existing-customers
          (let [prev-month (or (last prev-months)
                               {:total-customers (in! :starting-customers)})]
            (h/get! prev-month :total-customers)))

(property :new-customers
          (* (rs! :existing-customers)
             (/ (rs! :sales-growth-rate) 12)
             (rs! :pro-rata-factor)))

;; TODO: Fix lost customers.
(property :lost-customers
          0
          ;; (let [monthly-loss-rate
          ;;       (if-subscription
          ;;           (Math/pow (- 1 (/ (:retention-rate in) 100.0))
          ;;                     (/ 1.0 (:billing-cycles-per-year in)))

          ;;           (Math/pow (- 1 (/ (:transactions-per-year in) 100.0))
          ;;                     (/ 1.0 12.0)))]
          ;;   (* (:existing-customers rs) monthly-loss-rate (:pro-rata-factor rs)))
          )

(property :total-customers
          (- (+ (rs! :existing-customers) (rs! :new-customers)) (rs! :lost-customers)))


(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.customer-rows
   [:existing-customers :new-customers :lost-customers :total-customers]
   results prev-months month inputs))
