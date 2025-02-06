(ns reports-api.reports.sales-forecast.cost-and-profit-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

;; (defn calculate-customer-base [inputs results]
;;   (h/when-model inputs :purchase
;;               (let [{:keys [customer-activity-pattern]} inputs
;;                     {:keys [existing-customers sales-growth-rate pro-rata-factor]} results]
;;                 (* existing-customers sales-growth-rate customer-activity-pattern pro-rata-factor))))

(h/defn-pass-name calculate-cost-of-sales [fn-name inputs results]
  (let [{:keys []} inputs
        {:keys [units-sold cost]} results]
    ;; (h/assertions fn-name total-revenue [number?] "Total revenue must be a number")
    (* units-sold cost)))

(defn process [prev-months month inputs results]
  ;; TODO: Price (from the helpers Claude).
  (as-> (merge {:price 1 :total-revenue 1000 :active-customers 1} results) r
    (assoc r :cost-of-sales (calculate-cost-of-sales inputs r))))
