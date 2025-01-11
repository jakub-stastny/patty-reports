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

(def monthly-contribution-validator)

(def growth-curve-validator)

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]
    (-> {}
        (merge (v/validate-projections-keys inputs))

        (merge (validate-or-default :average-eu-vat-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :bad-debt-provision [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :customer-vat-status [customer-vat-status-validator] :all-vat-registered))
        (merge (validate-or-default :eu-vat-approach [eu-vat-approach-validator] :own-vat-returns))
        (merge (validate-or-default :registered-for-vat [v/boolean-validator] true))
        (merge (validate-or-default :vat-payment-frequency [v/optional-single-or-multiple-months-validator] nil))

        (merge (validate-or-default :offering-type [offering-type-validator] :product))
        (merge (validate-or-default :sales-start-date [v/timestamp-validator v/dt-converter] (t/years-from-now -10)))
        (merge (validate-or-default :sales-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10)))
        (merge (validate-or-default :revenue-model [revenue-model-validator] :purchase))
        (merge (validate-or-default :starting-customers [v/number-validator] 0))
        (merge (validate-or-default :typical-purchase-quantity [v/number-validator] 0))
        (merge (validate-or-default :annual-repeat-purchase [v/number-validator] 0))
        (merge (validate-or-default :yearly-purchase-frequency [v/number-validator] 0))
        (merge (validate :billing-cycles-per-year [v/single-or-multiple-months-or-weekly-or-daily-validator]))
        (merge (validate-or-default :retention-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :yoy-sales-growth [growth-curve-validator] [0 0 0 0 0]))
        (merge (validate-or-default :selling-price [v/number-validator] 0))
        (merge {:selling-price-changes (map v/validate-pay-change (or (:selling-price-changes inputs) []))})
        ;; :domestic-sales
        ;; :eu-sales
        ;; :rest-of-world-sales
        (merge (validate-or-default :refund-returns-allowance [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :sales-vat [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :payment-terms-sales [v/month-timing-validator] :same-month))
        (merge (validate-or-default :cost-of-sale [v/number-validator] 0))
        (merge {:cost-of-sale-changes (map v/validate-pay-change (or (:cost-of-sale-changes inputs) []))})
        (merge (validate-or-default :cost-vat [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :payment-terms-costs [v/month-timing-validator] :same-month))
        (merge (validate :monthly-contribution [monthly-contribution-validator])))))

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :handle inputs)))
