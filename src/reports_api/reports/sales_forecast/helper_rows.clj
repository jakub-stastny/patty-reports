(ns reports-api.reports.sales-forecast.helper-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(defn calculate-xxx [inputs results]
  (let [{:keys [registered-for-helper eu-vat-approach percentage-eu-customers-vat-registered sales-vat average-eu-vat-rate]} inputs
        {:keys [domestic-sales eu-sales]} results]
    ;; TODO
    ;; (if registered-for-helper)
    ))

(defn process [prev-months month inputs results]
  (as-> results results
    (assoc results :xxx (calculate-xxx inputs results))))
