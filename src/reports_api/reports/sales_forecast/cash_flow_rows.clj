(ns reports-api.reports.sales-forecast.cash-flow-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(h/calculate :sales-revenue-due
             (let [{:keys [payment-terms-sales]} inputs
                   ;; {:keys [total-revenue]} results
                   offset (case payment-terms-sales
                            :prev-month -1 :same-month 0 :next-month 1)]
               ;; (h/assert-number fn-name :total-revenue total-revenue)
               (:total-revenue (or (get prev-months offset) {:total-revenue 0}))))

(h/calculate :bad-debts
             (let [ ;;{:keys [bad-debt-provision]} inputs
                   {:keys [sales-revenue-due bad-debt-provision]} results]
               (h/assert-number fn-name :sales-revenue-due sales-revenue-due)
               (h/assert-number fn-name :bad-debt-provision bad-debt-provision)
               (* -1 sales-revenue-due bad-debt-provision)))

(h/calculate :sales-revenue-received
             (let [{:keys [sales-revenue-due bad-debts]} results]
               (+ sales-revenue-due bad-debts)))

;; Is this perhaps more explicit?
;; (h/calculate :sales-revenue-received
;;              (+ (:sales-revenue-due results) (:bad-debts results)))

(defn process [prev-months month inputs results]
  (h/calculate-properties
   'reports-api.reports.sales-forecast.cash-flow-rows
   [:sales-revenue-due :bad-debts :sales-revenue-received]
   (merge results {:cost 1}) prev-months month inputs))
