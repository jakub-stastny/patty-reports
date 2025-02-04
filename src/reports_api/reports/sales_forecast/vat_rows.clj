(ns reports-api.reports.sales-forecast.vat-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(defn calculate-vat-in [inputs results]
  (let [{:keys [costs registered-for-vat cost-vat]} inputs
        {:keys []} results]
    ;; TODO
    ;; (if registered-for-vat)
    0
    ))

(defn calculate-vat-out [inputs results]
  (let [{:keys [registered-for-vat eu-vat-approach percentage-eu-customers-vat-registered sales-vat average-eu-vat-rate]} inputs
        {:keys [domestic-sales eu-sales]} results]
    ;; TODO
    ;; (if registered-for-vat)
    0
    ))

(defn process [prev-months month inputs results]
  (as-> results r
    (assoc r :vat-in (calculate-vat-in inputs r))
    (assoc r :vat-out (calculate-vat-out inputs r))))
