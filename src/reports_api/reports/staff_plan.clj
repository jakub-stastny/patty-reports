;; https://docs.google.com/document/d/1clDJiSuQbARB1T8IuPnhjYDPIH2JcdLhJ-u9vfImTp8/edit?tab=t.0

(ns reports-api.reports.staff-plan
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]))

(def pay-structure-opts
  {"Annual Salary" :annual-salary "Monthly Salary" :monthly-salary
   "Weekly Salary" :weekly-salary "Hourly Rate" :hourly-rate})

;; Custom validators.
(def pay-structure-validator
  (v/make-validator :pay-structure
                    (str "must be one of: " (str/join "," (keys pay-structure-opts)))
                    (fn [v]
                      (when-let [matched-key (some #(and (= % v) %) (keys pay-structure-opts))]
                        (get pay-structure-opts matched-key)))))

(defn validate-pay-change [pc]
  {:effective-date (v/validate pc :effective-date [v/timestamp-validator v/dt-converter])
   :new-value (v/validate pc :new-value [v/number-validator])})

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]

    (-> {}
        (merge (validate-or-default :projections-duration [v/generate-range-validator 1 5] 1))
        (merge (validate-or-default :projections-start-date [v/timestamp-validator v/month-converter] (t/current-month)))

        (merge (validate-or-default :employment-start-date [v/timestamp-validator v/dt-converter] (t/years-from-now -10)))
        (merge (validate-or-default :employment-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10)))
        (merge (validate-or-default :number-of-hires [v/positive-number-validator] 1))
        (merge (validate-or-default :work-weeks-per-year [v/number-validator] 52))
        (merge (validate-or-default :work-hours-per-week [v/number-validator] 40))
        (merge (validate :base-pay [v/number-validator]))
        (merge (validate-or-default :business-function [v/string-validator] nil))
        (merge (validate :pay-structure [pay-structure-validator]))
        (merge (validate-or-default :benefits-allowance [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :employer-tax-rate [(v/generate-range-validator 0 1)] 0))
        (merge {:pay-changes (map validate-pay-change (or (:pay-changes inputs) []))}))))

(defn calculate-pro-rata-base-pay [rate current-month-pay-changes]
  (let [rates
        (map (fn [pc]
               {:since (.getDayOfMonth (:effective-date pc)) :rate (:new-value pc)})
             current-month-pay-changes)]
    (if (some #(= 1 (:since %)) rates)
      rates
      (into [{:since 1 :rate rate}] rates))))

(defn find-last-pay-change-before-current-month [month pay-changes]
  (let [prev-changes
        (filter #(= 1 (t/compare-month month (t/extract-year-and-month (:effective-date %)))) pay-changes)]
    (last (sort-by :effective-date prev-changes))))

(defn filter-current-month-pay-changes [month pay-changes]
  (filter #(= 0 (t/compare-month month (t/extract-year-and-month (:effective-date %)))) pay-changes))

;; TODO: Also consider employment-start-date/employment-end-date.
;; Just put {:since n :rate 0}
(defn calculate-current-rates [month {:keys [base-pay pay-changes employment-start-date employment-end-date]}]
  (let [last-pay-change-before-current-month
        (find-last-pay-change-before-current-month month pay-changes)

        current-base-pay-rate
        (if last-pay-change-before-current-month
          (:new-value last-pay-change-before-current-month)
          base-pay)

        current-month-pay-changes
        (filter-current-month-pay-changes month pay-changes)]
    (cond
      (empty? current-month-pay-changes)
      [{:since 1 :rate current-base-pay-rate}]

      (seq current-month-pay-changes)
      (calculate-pro-rata-base-pay current-base-pay-rate current-month-pay-changes)

      :else
      (throw (ex-info "Unhandled clause"
                      {:fn :calculate-current-base-pay :month month
                       :base-pay base-pay :pay-changes pay-changes})))))

;; Converts: [{:since 1, :rate 90} {:since 13, :rate 100}]
;; to:       [{:days 12 :rate 90}  {:days 18 :rate 100}]
(defn convert-rates-to-ratios [rates]
  (let [days-in-month 30]
    (->> (partition 2 1 (concat rates [nil]))
         (map (fn [[current next]]
                (if current
                  {:days (if next
                           (- (:since next) (:since current))
                           (- days-in-month (:since current) -1))
                   :rate (:rate current)})))
         (remove nil?))))

(defn calculate-monthly-pay [month {:keys [work-weeks-per-year work-hours-per-week pay-structure] :as inputs}]
  ;; TODO: implement pro-rata.
  (let [current-rates (calculate-current-rates month inputs)
        current-ratios (convert-rates-to-ratios current-rates)
        percentage-of-working-time (/ 52 work-weeks-per-year)]
    ;; (prn :cr current-rates current-ratios)
    (case pay-structure
      :hourly-rate (let [rate (:rate (first current-rates))]
                     ;; (when (> (count current-rates) 1)
                     ;;   (prn :hr month current-rates)) ;;;;;;;
                     (int (* work-hours-per-week rate percentage-of-working-time)))

      :weekly-salary (let [rate (:rate (first current-rates))
                           ;; work-days-per-month (/ (* 365 (/ 5 7)) 12)
                           ;; weeks-per-month (/ work-days-per-month 5)
                           weeks-per-month (/ 52 12)]
                       (int (* weeks-per-month rate percentage-of-working-time)))

      :monthly-salary (let [rate (:rate (first current-rates))]
                        (int (* rate percentage-of-working-time)))

      :annual-salary (let [rate (:rate (first current-rates))]
                       (int (* (/ rate 12) percentage-of-working-time))))))

(defn generate-report-month [month {:keys [number-of-hires benefits-allowance employer-tax-rate] :as inputs}]
  (let [monthly-pay (* (calculate-monthly-pay month inputs) number-of-hires)
        benefits (* monthly-pay benefits-allowance)
        employer-payroll-tax (* monthly-pay employer-tax-rate)
        staff-cost (+ monthly-pay benefits employer-payroll-tax)]
    {:month (t/format-month month) :timestamp (t/month-to-ts month) :monthly-pay monthly-pay
     :benefits benefits :employer-payroll-tax employer-payroll-tax :staff-cost staff-cost}))

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :clean-inputs inputs)
    (first (reduce (fn [[report month] _]
                     [(conj report (generate-report-month month inputs)) (t/next-month month)])
                   [[] projections-start-date]
                   (repeat (* projections-duration 12) nil)))))
