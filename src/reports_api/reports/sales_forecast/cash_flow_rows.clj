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

(defn process [prev-months month inputs results]
  ;; TODO: Cost (from the helpers Claude).
  (as-> (merge {:cost 1} results) r
    (assoc r :sales-revenue-due (calculate-sales-revenue-due prev-months inputs r))))
