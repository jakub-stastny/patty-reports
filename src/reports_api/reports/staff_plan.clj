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

(defn validate-inputs [raw-inputs]
  (let [inputs (h/transform-keys-to-kebab-case raw-inputs)
        validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]

    (-> {}
        (merge (validate-or-default :projections-duration [v/generate-range-validator 1 5] 1))
        (merge (validate-or-default :projections-start-date [v/timestamp-validator v/dt-converter] (t/now)))

        (merge (validate-or-default :employment-start-date [v/timestamp-validator v/dt-converter] (t/years-from-now -10)))
        (merge (validate-or-default :employment-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10)))
        (merge (validate-or-default :number-of-hires [v/positive-number-validator] 1))

        (merge (validate-or-default :work-weeks-per-year [v/number-validator] 0))
        (merge (validate-or-default :work-hours-per-week [v/number-validator] 0))
        (merge (validate-or-default :base-pay [v/number-validator] 0))
        (merge (validate-or-default :business-function [v/string-validator] nil))
        (merge (validate :pay-structure [pay-structure-validator]))
        (merge (validate-or-default :benefits-allowance [(v/generate-range-validator 0 1)] 0))
        (merge (map validate-pay-change (or (:pay-changes inputs) []))))

    ;; {:projections-duration (v/validate-or-default inputs :projections-duration [v/number-validator (v/generate-range-validator 1 5)] 1)
    ;;  :projections-start-date (v/validate-or-default inputs :projections-start-date [v/timestamp-validator v/dt-converter] (t/now))
    ;;  :employment-start-date (v/validate inputs :employment-start-date [v/timestamp-validator v/dt-converter])
    ;;  :employment-end-date (v/validate-or-default inputs :employment-end-date [v/timestamp-validator v/dt-converter] (t/years-from-now 10))
    ;;  :number-of-hires (v/validate-or-default inputs :number-of-hires [v/positive-number-validator] 1)
    ;;  :work-weeks-per-year (v/validate-or-default inputs :work-weeks-per-year [v/number-validator] 0)
    ;;  :work-hours-per-week (v/validate-or-default inputs :work-hours-per-week [v/number-validator] 0)
    ;;  :base-pay (v/validate-or-default inputs :base-pay [v/number-validator] 0)
    ;;  :business-function (v/validate-or-default inputs :business-function [v/string-validator] nil)
    ;;  :pay-structure (v/validate inputs :pay-structure [pay-structure-validator])
    ;;  :benefits-allowance (v/validate-or-default inputs :benefits-allowance [(v/generate-range-validator 0 1)] 0)
    ;;  :pay-changes (map validate-pay-change (or (:pay-changes inputs) []))}
    ))

(defn handle [raw-inputs]
  (let [{:keys [number-of-hires] :as inputs} (validate-inputs raw-inputs)]
    (prn :validated-inputs inputs)
    {:done "OK"}))
