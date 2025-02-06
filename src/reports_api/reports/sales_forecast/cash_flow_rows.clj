(ns reports-api.reports.sales-forecast.cash-flow-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(h/defn-pass-name calculate-cost-of-sales [fn-name inputs results]
  (let [{:keys []} inputs
        {:keys [units-sold cost]} results]
    (h/assert-number fn-name :units-sold units-sold)
    (h/assert-number fn-name :cost cost)
    (* units-sold cost)))

(defn process [prev-months month inputs results]
  ;; TODO: Cost (from the helpers Claude).
  (as-> (merge {:cost 1} results) r
    (assoc r :cost-of-sales (calculate-cost-of-sales inputs r))))
