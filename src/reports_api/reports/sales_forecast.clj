(ns reports-api.reports.sales-forecast
  (:require [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]
            [clojure.string :as str]))

(def eu-vat-approach-validator
  (v/generate-options-validator
   :eu-vat-approach
   {"Own VAT Returns" :own-vat-returns
    "Outsourced VAT management" :outsourced-vat-management}))

(def offering-type-validator
  (v/generate-options-validator
   :offering-type {"Product" :product "Service" :service}))

(def customer-vat-status-validator
  (v/generate-options-validator
   :customer-vat-status
   {"All customers VAT registered" :all-vat-registered
    "Some customers VAT registered" :some-vat-registered}))

(def revenue-model-validator
  (v/generate-options-validator :revenue-model
   {"One-time purchase" :purchase "Subscription" :subscription}))

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]
    (-> {}
        (merge (v/validate-projections-keys inputs))
        (merge (validate-or-default :sales-start-date [v/timestamp-validator v/dt-converter] (t/years-from-now -10)))
        (merge (validate-or-default :sales-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10)))
        (merge (validate-or-default :offering-type [offering-type-validator] :product))
        (merge (validate-or-default :revenue-model [revenue-model-validator] :purchase))
        (merge (validate-or-default :customer-vat-status [customer-vat-status-validator] :all-vat-registered))
        (merge (validate-or-default :average-eu-vat-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :bad-debt-provision [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :eu-vat-approach [eu-vat-approach-validator] :own-vat-returns))
        (merge (validate-or-default :starting-customers [v/number-validator] 0))
        (merge (validate-or-default :typical-purchase-quantity [v/number-validator] 0))
        (merge (validate-or-default :annual-repeat-purchase [v/number-validator] 0))
        (merge (validate-or-default :yearly-purchase-frequency [v/number-validator] 0))
        (merge (validate-or-default :registered-for-vat [v/boolean-validator] true))
        (merge (validate-or-default :vat-payment-months [v/optional-single-or-multiple-months-validator] nil)))))

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :handle inputs)))
