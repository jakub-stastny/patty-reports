;; https://docs.google.com/document/d/1clDJiSuQbARB1T8IuPnhjYDPIH2JcdLhJ-u9vfImTp8/edit?tab=t.0

(ns reports-api.reports.staff-plan
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
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

;; projections_start_date: UNIX timestamp in ms
;; employment_start_date: UNIX timestamp in ms
;; employment_end_date: UNIX timestamp in ms
;; benefits_allowance_%: expressed as a decimal

;; pay_changes.effective_data: UNIX timestamp in ms
;; pay_changes.new_value: number
(defn validate-inputs [raw-inputs]
  (let [inputs (h/transform-keys-to-kebab-case raw-inputs)]
    {:projections-duration (v/validate-or-default inputs :projections-duration [v/number-validator (v/generate-range-validator 1 5)] 1)
     :number-of-hires (v/validate-or-default inputs :number-of-hires [v/positive-number-validator] 1)
     :work-weeks-per-year (v/validate-or-default inputs :work-weeks-per-year [v/number-validator] 0)
     :work-hours-per-week (v/validate-or-default inputs :work-hours-per-week [v/number-validator] 0)
     :base-pay (v/validate-or-default inputs :base-pay [v/number-validator] 0)
     :business-function (v/validate-or-default inputs :business-function [v/string-validator] nil)
     :pay-structure (v/validate inputs :pay-structure [pay-structure-validator])
     :benefits-allowance-% (v/validate-or-default inputs :benefits-allowance-% [(v/generate-range-validator 0 1)] 0)}))

(defn handle [raw-inputs]
  (let [{:keys [number-of-hires] :as inputs} (validate-inputs raw-inputs)]
    (prn :validated-inputs inputs)
    {:done "OK"}))
