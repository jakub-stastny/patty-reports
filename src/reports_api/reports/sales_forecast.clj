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

(def monthly-contribution-validator
  (v/make-validator
   :monthly-contribution
   "must be an array of 12 numbers between 0 and 1 that sum to 1.0"
   (fn [v]
     (when (and (sequential? v)
                (= 12 (count v))
                (every? number? v)
                (every? #(<= 0 % 1) v)
                (= 1.0 (double (reduce + v))))
       v))))

(def growth-curve-validator
  (v/make-validator
   :growth-curve
   "must be an array of up to 5 numbers between -0.1 and 5"
   (fn [v]
     (when (and (sequential? v)
                (<= (count v) 5)
                (every? number? v)
                (every? #(<= -0.1 % 5) v))
       v))))

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
        (merge (validate-or-default :billing-cycles-per-year [v/single-or-multiple-months-or-weekly-or-daily-validator] 1))
        (merge (validate-or-default :retention-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :yoy-sales-growth [growth-curve-validator] [0 0 0 0 0]))
        (merge (validate-or-default :selling-price [v/number-validator] 0))
        (merge {:selling-price-changes (map v/validate-pay-change (or (:selling-price-changes inputs) []))})

        ;; Each array position represents the percentage of total
        ;; sales in that market for years 1-5. The company starts with
        ;; 100% domestic sales, enters the EU in year 2 capturing 20%
        ;; of sales, then expands to other international markets in
        ;; year 3. By year 5, sales are split 45% domestic, 35% EU,
        ;; and 20% rest of world.
        (merge (validate-or-default :domestic-sales [growth-curve-validator] [1.00, 0.80, 0.65, 0.55, 0.45]))
        (merge (validate-or-default :eu-sales [growth-curve-validator] [0.00, 0.20, 0.25, 0.30, 0.35]))
        (merge (validate-or-default :rest-of-world-sales [growth-curve-validator] [0.00, 0.00, 0.10, 0.15, 0.20]))

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
