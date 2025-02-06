(ns reports-api.reports.sales-forecast.cost-and-profit-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(h/defn-pass-name calculate-cost-of-sales [fn-name _ _ inputs results]
  (let [{:keys []} inputs
        {:keys [units-sold cost]} results]
    (h/assert-number fn-name :units-sold units-sold)
    (h/assert-number fn-name :cost cost)
    (* units-sold cost)))

(h/defn-pass-name calculate-bad-debt-provision [fn-name _ _ inputs results]
  (let [{:keys [bad-debt-provision]} inputs
        {:keys [total-revenue]} results]
    (h/assert-number fn-name :bad-debt-provision bad-debt-provision)
    (h/assert-number fn-name :total-revenue total-revenue)
    (* -1 bad-debt-provision total-revenue)))

(h/defn-pass-name calculate-gross-profit [fn-name _ _ inputs results]
  (let [{:keys []} inputs
        {:keys [net-revenue cost bad-debt-provision]} results]
    (h/assert-number fn-name :net-revenue net-revenue)
    (h/assert-number fn-name :cost cost)
    (h/assert-number fn-name :bad-debt-provision bad-debt-provision)
    (+ net-revenue cost bad-debt-provision)))

(defn process [prev-months month inputs results]
  (h/calculate-properties
   'reports-api.reports.sales-forecast.cost-and-profit-rows
   [:cost-of-sales :bad-debt-provision :gross-profit]
   (merge results {:cost 1 :net-revenue 200}) prev-months month inputs))
