(ns reports-api.reports.sales-forecast.vat-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h :refer [calc-prop] :rename {calc-prop property}]
            [reports-api.time :as t]))

;; TODO
(property :vat-in
          (if (:registered-for-vat in)
            (* (:cost-vat in) (:costs rs))
            0))

;; TODO
;; {:keys [registered-for-vat eu-vat-approach percentage-eu-customers-vat-registered sales-vat average-eu-vat-rate]} inputs
;; {:keys [domestic-sales eu-sales]} results
(property :vat-out
          (if (:registered-for-vat in)
            (* (:cost-vat in) (:costs rs))
            0))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.vat-rows
   [:vat-in :vat-out]
   results prev-months month inputs))
