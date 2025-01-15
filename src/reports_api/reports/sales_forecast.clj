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

(defn generate-yoy-fulfills-projections-duration [c]
  (v/make-validator
   :yoy-fulfills-projections-duration
   (str "must be a sequential of at least " c " items")
   #(and (<= c (count %)) %)))

(defn validate-inputs [inputs]
  (let [validate (fn [state & args] (apply v/validate state inputs args))]
    (v/ensure-valid
     (as-> {:errors {} :data {}} s
         (v/validate-projections-keys s inputs)
         (validate s :average-eu-vat-rate [(v/generate-range-validator 0 1)] 0)
         (validate s :bad-debt-provision [(v/generate-range-validator 0 1)] 0)
         (validate s :customer-vat-status [customer-vat-status-validator] :all-vat-registered)
         (validate s :eu-vat-approach [eu-vat-approach-validator] :own-vat-returns)
         (validate s :registered-for-vat [v/boolean-validator] true)
         (validate s :vat-payment-frequency [v/optional-single-or-multiple-months-validator] nil)

         (validate s :sales-start-date [v/timestamp-validator v/dt-converter] (t/years-from-now -10))
         (validate s :sales-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10))

         (validate s :offering-type [offering-type-validator] :product)
         (validate s :revenue-model [revenue-model-validator] :purchase)
         (validate s :starting-customers [v/number-validator] 0)
         (validate s :typical-purchase-quantity [v/number-validator] 0)
         (validate s :annual-repeat-purchase [v/number-validator] 0)
         (validate s :yearly-purchase-frequency [v/number-validator] 0)
         (validate s :billing-cycles-per-year [v/single-or-multiple-months-or-weekly-or-daily-validator] 1)
         (validate s :retention-rate [(v/generate-range-validator 0 1)] 0)
         (validate s :yoy-sales-growth
                   [growth-curve-validator
                    (generate-yoy-fulfills-projections-duration
                     (get-in s [:data :projections-duration]))]
                   (vec (repeat 5 0)))
         (validate s :selling-price [v/number-validator] 0)
         (v/validate-rate-changes s inputs :selling-price-changes)

         ;; Each array position represents the percentage of total
         ;; sales in that market for years 1-5. The company starts with
         ;; 100% domestic sales, enters the EU in year 2 capturing 20%
         ;; of sales, then expands to other international markets in
         ;; year 3. By year 5, sales are split 45% domestic, 35% EU,
         ;; and 20% rest of world.
         (validate s :domestic-sales [growth-curve-validator] [1.00 0.80 0.65 0.55 0.45])
         (validate s :eu-sales [growth-curve-validator] [0.00 0.20 0.25 0.30 0.35])
         (validate s :rest-of-world-sales [growth-curve-validator] [0.00 0.00 0.10 0.15 0.20])

         (validate s :refund-returns-allowance [(v/generate-range-validator 0 1)] 0)
         (validate s :sales-vat [(v/generate-range-validator 0 1)] 0)
         (validate s :payment-terms-sales [v/month-timing-validator] :same-month)
         (validate s :cost-of-sale [v/number-validator] 0)
         (v/validate-rate-changes s inputs :cost-of-sale-changes)
         (validate s :cost-vat [(v/generate-range-validator 0 1)] 0)
         (validate s :payment-terms-costs [v/month-timing-validator] :same-month)
         (validate s :monthly-contribution [monthly-contribution-validator] (vec (repeat 5 0)))))))

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :clean-inputs inputs)
    {:status "OK"}))
