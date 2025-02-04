(ns reports-api.reports.sales-forecast.vat-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(defn calculate-vat-in [inputs results]
  (let [{:keys [costs registered-for-vat cost-vat]} inputs
        {:keys []} results]
    ;; TODO
    ;; (if registered-for-vat)
    ))

(defn calculate-vat-out [inputs results]
  (let [{:keys [registered-for-vat eu-vat-approach percentage-eu-customers-vat-registered sales-vat average-eu-vat-rate]} inputs
        {:keys [domestic-sales eu-sales]} results]
    ;; TODO
    ;; (if registered-for-vat)
    ))

(defn vat-rows [prev-months month inputs results]
  (as-> (merge {:price 1 :total-vat 1000} results) results
    (assoc results :vat-in (calculate-vat-in inputs results))
    (assoc results :vat-out (calculate-vat-out inputs results))))

(def vat-keys [:vat-in :vat-out])
