(ns reports-api.reports.sales-forecast.cost-and-profit-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(h/defn-pass-name calculate-cost-of-sales [fn-name inputs results]
  (let [{:keys []} inputs
        {:keys [units-sold cost]} results]
    (h/assert-number fn-name :units-sold units-sold)
    (h/assert-number fn-name :cost cost)
    (* units-sold cost)))

(h/defn-pass-name calculate-bad-debt-provision [fn-name inputs results]
  (let [{:keys [bad-debt-provision]} inputs
        {:keys [total-revenue]} results]
    (h/assert-number fn-name :bad-debt-provision bad-debt-provision)
    (h/assert-number fn-name :total-revenue total-revenue)
    (* -1 bad-debt-provision total-revenue)))

(defn process [prev-months month inputs results]
  ;; TODO: Cost (from the helpers Claude).
  (as-> (merge {:cost 1} results) r
    (assoc r :cost-of-sales (calculate-cost-of-sales inputs r))
    (assoc r :bad-debt-provision (calculate-bad-debt-provision inputs r))))
