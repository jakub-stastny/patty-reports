(ns reports-api.reports.sales-forecast.cash-flow-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.time :as t]))

(property :sales-revenue-due
          (let [offset
                (case (:payment-terms-sales in)
                  :prev-month -1 :same-month 0 :next-month 1)]
            (:total-revenue (or (get prev-months offset)
                                {:total-revenue 0}))))

(property :bad-debt-provision
          (* -1 (:sales-revenue-due rs) (:bad-debt-provision in)))

(property :sales-revenue-received
          (+ (:sales-revenue-due rs) (:bad-debts rs)))

;; 6.2 Bad Debts (Cash Impact)
(property :bad-debts
          (* -1 (:bad-debt-provision rs) (:sales-revenue-due rs)))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.cash-flow-rows
   [:sales-revenue-due :bad-debts :sales-revenue-received]
   (merge results {:cost 1}) prev-months month inputs))
