(ns reports-api.reports.sales-forecast
  (:require [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]))

(def customer-vat-status-opts
  {"All customers VAT registered" :all-vat-registered 
   "Some customers VAT registered" :some-vat-registered})

(def customer-vat-status-validator
  (v/make-validator :customer-vat-status
                   (str "must be one of: " (str/join "," (keys customer-vat-status-opts)))
                   (fn [v]
                     (when-let [matched-key (some #(and (= % v) %) (keys customer-vat-status-opts))]
                       (get customer-vat-status-opts matched-key)))))

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]
    (-> {}
        (merge (v/validate-projections-keys inputs))
        (merge (validate-or-default :customer-vat-status [customer-vat-status-validator] :all-vat-registered))
        (merge (validate-or-default :average-eu-vat-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :bad-debt-provision [(v/generate-range-validator 0 1)] 0)))))

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :handle inputs)))
