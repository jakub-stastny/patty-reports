(ns reports-api.reports.sales-forecast.cash-flow-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(h/defn-pass-name calculate-sales-revenue-due [fn-name prev-months inputs results]
  (let [{:keys [payment-terms-sales]} inputs
        ;; {:keys [total-revenue]} results
        offset (case payment-terms-sales
                 :prev-month -1 :same-month 0 :next-month 1)]
    ;; (h/assert-number fn-name :total-revenue total-revenue)
    (:total-revenue (or (get prev-months offset) {:total-revenue 0}))))

(h/defn-pass-name calculate-bad-debts [fn-name inputs results]
  (let [;;{:keys [bad-debt-provision]} inputs
        {:keys [sales-revenue-due bad-debt-provision]} results]
    (h/assert-number fn-name :sales-revenue-due sales-revenue-due)
    (h/assert-number fn-name :bad-debt-provision bad-debt-provision)
    (* -1 revenue-due bad-debt-provision)))

(defn process [prev-months month inputs results]
  ;; TODO: Cost (from the helpers Claude).
  (as-> (merge {:cost 1} results) r
    (assoc r :sales-revenue-due (calculate-sales-revenue-due prev-months inputs r))
    (assoc r :bad-debts (calculate-bad-debts inputs r))))
