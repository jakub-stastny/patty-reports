(ns reports-api.reports.staff-plan
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]
            [reports-api.totals :as tot]
            [reports-api.bubble :as b]
            [reports-api.pro-rata-engine :as pr]))

;; Custom validators.
(def pay-structure-validator
  (v/generate-options-validator
   :pay-structure {"Hourly Rate" :hourly-rate
                   "Weekly Salary" :weekly-salary
                   "Monthly Salary" :monthly-salary
                   "Annual Salary" :annual-salary}))

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]

    (-> {}
        (merge (v/validate-projections-keys inputs))

        (merge (validate-or-default :employment-start-date [v/timestamp-validator v/dt-converter] (t/years-from-now -10)))
        (merge (validate-or-default :employment-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10)))
        (merge (validate-or-default :number-of-hires [v/positive-number-validator] 1))
        (merge (validate-or-default :work-weeks-per-year [v/number-validator] 52))
        (merge (validate-or-default :work-hours-per-week [v/number-validator] 40))
        (merge (validate :base-pay [v/number-validator]))
        (merge (validate-or-default :business-function [v/string-validator] nil))
        (merge (validate :pay-structure [pay-structure-validator]))

        (merge (validate-or-default :benefits-allowance [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :benefits-payment-frequency [v/single-or-multiple-months-validator] (into (sorted-set) (range 1 13))))

        (merge (validate-or-default :employer-tax-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :month-timing [v/month-timing-validator] :same-month))
        (merge {:pay-changes (map v/validate-pay-change (or (:pay-changes inputs) []))}))))

(defn calculate-monthly-pay [month {:keys [work-weeks-per-year work-hours-per-week pay-structure] :as inputs}]
  (let [current-rates (pr/calculate-current-rates month inputs)
        current-ratios (pr/convert-rates-to-ratios current-rates)
        percentage-of-working-time (/ 52 work-weeks-per-year)]
    ;; TODO: Refactor this.
    (case pay-structure
      :hourly-rate (int (reduce (fn [acc {:keys [days rate]}]
                                  (let [pro-rata-ratio (/ days 30)]
                                    (+ acc
                                       (* pro-rata-ratio work-hours-per-week rate percentage-of-working-time))))
                                0 current-ratios))

      :weekly-salary (let [weeks-per-month (/ 52 12)]
                       (int
                        (reduce (fn [acc {:keys [days rate]}]
                                  (let [pro-rata-ratio (/ days 30)]
                                    (+ acc
                                       (* pro-rata-ratio weeks-per-month rate percentage-of-working-time))))
                                0 current-ratios)))

      :monthly-salary (int
                       (reduce (fn [acc {:keys [days rate]}]
                                 (let [pro-rata-ratio (/ days 30)]
                                   (+ acc
                                      (* pro-rata-ratio rate percentage-of-working-time))))
                               0 current-ratios))

      :annual-salary (int
                      (reduce (fn [acc {:keys [days rate]}]
                                (let [pro-rata-ratio (/ days 30)]
                                  (+ acc
                                     (* pro-rata-ratio (/ rate 12) percentage-of-working-time))))
                              0 current-ratios)))))

(defn calculate-payroll-tax [months-till-current employer-tax-rate month-timing]
  (case month-timing
    ;; We just make an assumption that it is the same as the current
    ;; month. Not accurate but close enough. Anything else is what we
    ;; call spurious accuracy. The only thing to watch for is the
    ;; number of days worked.
    :prev-month
    (* (:monthly-pay (last months-till-current)) employer-tax-rate)

    :same-month
    (* (:monthly-pay (last months-till-current)) employer-tax-rate)

    :following-month
    (* (:monthly-pay (h/penultimate-or-first months-till-current)) employer-tax-rate)

    :last-month-of-quarter
    (if (contains? #{3 6 9 12} (:month (:month (last months-till-current))))
      (let [last-3-months
            (h/last-3-items
             (into [(first months-till-current) (first months-till-current) (first months-till-current)]
                   months-till-current))]
        (reduce (fn [acc {:keys [monthly-pay]}]
                  (+ acc (* monthly-pay employer-tax-rate)))
                0 last-3-months))

      ;; No tax paid out this month.
      0)

    :month-following-end-of-quarter
    (if (contains? #{1 4 7 10} (:month (:month (last months-till-current))))
      (let [prev-3-months
            (h/last-3-penultimate (into [(first months-till-current)
                                         (first months-till-current)
                                         (first months-till-current)]
                                        months-till-current))]
        (reduce (fn [acc {:keys [monthly-pay]}]
                  (+ acc (* monthly-pay employer-tax-rate)))
                0 prev-3-months))

      ;; No tax paid out this month.
      0)))

(defn calculate-benefits [months-till-current benefits-allowance benefits-payment-frequency]
  (if (contains? benefits-payment-frequency (:month (:month (last months-till-current))))
    (let [last-month-benefits-paid-out (h/get-prev-item benefits-payment-frequency (:month (:month (last months-till-current))))
          offset-3-months-till-current (into [(first months-till-current)
                                              (first months-till-current)
                                              (first months-till-current)]
                                             months-till-current)

          month-range-for-benefits (h/select-months-until
                                    offset-3-months-till-current
                                    last-month-benefits-paid-out)]

      (reduce (fn [acc {:keys [monthly-pay]}]
                (+ acc (* monthly-pay benefits-allowance)))
              0 month-range-for-benefits))

    ;; No benefits paid out this month.
    0))

(defn generate-report-month [prev-months month
                             {:keys [number-of-hires benefits-allowance employer-tax-rate
                                     month-timing benefits-payment-frequency] :as inputs}]
  (let [monthly-pay (* (calculate-monthly-pay month inputs) number-of-hires)
        months-till-current (conj prev-months {:month month :monthly-pay monthly-pay})

        benefits (calculate-benefits months-till-current benefits-allowance benefits-payment-frequency)
        payroll-tax (calculate-payroll-tax months-till-current employer-tax-rate month-timing)

        staff-cost (+ monthly-pay benefits payroll-tax)]
    {:month month :timestamp (t/month-to-ts month) :monthly-pay monthly-pay
     :benefits benefits :payroll-tax payroll-tax :staff-cost staff-cost}))

(defn generate-projections [projections-start-date projections-duration inputs]
  (first (reduce (fn [[report month] _]
                   [(conj report (generate-report-month report month inputs)) (t/next-month month)])
                 [[] projections-start-date]
                 (repeat (* projections-duration 12) nil))))

(defn handle [raw-inputs]
  (tot/add-yearly-totals-one
   (b/format-for-bubble-one
    (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
      ;; (prn :clean-inputs inputs)
      (generate-projections projections-start-date projections-duration inputs)))))
