(ns reports-api.reports.sales-forecast
  (:require [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]
            [reports-api.xhelpers :as xh]
            [reports-api.totals :as tot]
            [reports-api.bubble :as b]
            [clojure.string :as str]))

(def eu-vat-approach-validator
  (v/generate-options-validator
   :eu-vat-approach
   {"Own VAT Returns" :own-vat-returns
    "Outsourced VAT management" :outsourced-vat-management}))

(def offering-type-validator
  (v/generate-options-validator
   :offering-type {"Product" :product "Service" :service}))

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

;; TODO: a lot of this is under offerings and need to be validated as pay-changes.
(defn validate-inputs [inputs]
  (let [validate (fn [state & args] (apply v/validate state inputs args))
        months-12 (into (sorted-set) (range 1 13))]
    (v/ensure-valid
     (as-> {:errors {} :data {}} s
         (v/validate-projections-keys s inputs)
         (validate s :average-eu-vat-rate [(v/generate-range-validator 0 1)] 0)
         (validate s :bad-debt-provision [(v/generate-range-validator 0 1)] 0)
         (validate s :percentage_eu_customers_vat_registered [(v/generate-range-validator 0 1)] 0)
         (validate s :eu-vat-approach [eu-vat-approach-validator] :own-vat-returns)
         (validate s :registered-for-vat [v/boolean-validator] true)
         (validate s :vat-payment-frequency [v/optional-single-or-multiple-months-validator] nil)
         (validate s :vat-payment-months [v/single-or-multiple-months-or-weekly-or-daily-validator] months-12)

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
         (validate s :monthly-contribution [monthly-contribution-validator] (vec (repeat 12 (/ 1.0 12))))))))

(defn generate-report-month [prev-months month {:keys [yoy-sales-growth starting-customers monthly-contribution] :as inputs}]
  (let [year-index (int (/ (count prev-months) 12))
        sales-growth-rate (nth yoy-sales-growth year-index)
        last-month-customers (:customers (or (last prev-months) {:customers starting-customers}))
        current-month-customers (* last-month-customers (+ 1 (/ sales-growth-rate 12)))]
    {:sales-growth-rate sales-growth-rate :customers current-month-customers}))

(def xkeys [:sales-growth-rate :customers])

(defn handle [raw-inputs]
  (let [inputs (validate-inputs raw-inputs)
        _ (prn :inputs inputs)
        projections (xh/generate-projections inputs generate-report-month)]
    (println) (prn :projections projections)
    (tot/add-yearly-totals-one
     (b/format-for-bubble-one projections (conj xkeys :timestamp))
     xkeys)))
