(ns reports-api.reports.sales-forecast.vat-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(h/calculate :vat-in
             (let [{:keys [costs registered-for-vat cost-vat]} inputs
                   {:keys []} results]
               ;; TODO
               ;; (if registered-for-vat)
               0))

(h/calculate :vat-out
             (let [{:keys [registered-for-vat eu-vat-approach percentage-eu-customers-vat-registered sales-vat average-eu-vat-rate]} inputs
                   {:keys [domestic-sales eu-sales]} results]
               ;; TODO
               ;; (if registered-for-vat)
               0))

(defn process [prev-months month inputs results]
  (h/calculate-properties
   'reports-api.reports.sales-forecast.vat-rows
   [:vat-in :vat-out]
   results prev-months month inputs))
