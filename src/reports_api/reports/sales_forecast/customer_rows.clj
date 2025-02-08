(ns reports-api.reports.sales-forecast.customer-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.time :as t]))

(property :existing-customers
          (:total-customers (or (last prev-months)
                                {:total-customers (:starting-customers in)})))

(property :new-customers (* (:existing-customers rs)
                            (/ (:sales-growth-rate in) 12)
                            (:pro-rata-factor rs)))

(property :lost-customers
          (let [monthly-loss-rate
                (if-subscription
                    (Math/pow (- 1 (/ (:retention-rate in) 100.0))
                              (/ 1.0 (:billing-cycles-per-year in)))

                    (Math/pow (- 1 (/ (:transactions-per-year in) 100.0))
                              (/ 1.0 12.0)))]
            (* (:existing-customers rs) monthly-loss-rate (:pro-rata-factor rs))))

(property :total-customers
          (- (+ (:existing-customers rs) (:new-customers rs)) (:lost-customers rs)))


(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.customer-rows
   [:existing-customers :new-customers :lost-customers :total-customers]
   (merge results {:price 1}) prev-months month inputs))
