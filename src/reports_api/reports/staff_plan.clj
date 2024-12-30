(ns reports-api.reports.staff-plan
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.validators :as v]))

;; https://docs.google.com/document/d/1clDJiSuQbARB1T8IuPnhjYDPIH2JcdLhJ-u9vfImTp8/edit?tab=t.0
;;
;; Projections variables
;; projections_start_date: UNIX timestamp in ms
;; projections_duration: number between one and five being the number of years
;;
;; Team variables
;; number_of_hires: number
;; employment_start_date: UNIX timestamp in ms
;; employment_end_date: UNIX timestamp in ms
;; pay_structure: "one of the following - Annual Salary, Monthly Salary, Weekly Salary, Hourly Rate"
;; work_hours_per_week: number
;; work_weeks_per_year: number
;; base_pay: number
;; benefits_allowance_%: expressed as a decimal
;; pay_changes.effective_data: UNIX timestamp in ms
;; pay_changes.new_value: number

(def defaults {:projections-duration 1 :number-of-hires 1})
(def pay-structure-opts {"Annual Salary" :annual-salary "Monthly Salary" :monthly-salary "Weekly Salary" :weekly-salary "Hourly Rate" :hourly-rate})


(def pay-structure-validator
  (v/make-validator :pay-structure
                    (str "must be one of: " (str/join "," (keys pay-structure-opts)))
                    (fn [v]
                      (when-let [matched-key (some #(and (= % v) %) (keys pay-structure-opts))]
                        (prn :when-let (get pay-structure-opts matched-key)) ;;;;
                        (get pay-structure-opts matched-key)))))

(defn validate-inputs [raw-inputs]
  (let [inputs (merge defaults (h/transform-keys-to-kebab-case raw-inputs))]
    (if (:contains? inputs :projections-duration)
      (v/validate inputs :projections-duration [v/number-validator {:type :1-to-5 :validator #(and (<= 1 % 5) %) :message "must be between 1 and 5"}]))

    (if (:contains? inputs :number-of-hires)
      (v/validate inputs :number-of-hires [v/number-validator]))

    (prn :x (v/validate inputs :pay-structure [pay-structure-validator]))

    inputs))

(defn handle [raw-inputs]
  (let [{:keys [number-of-hires] :as inputs} (validate-inputs raw-inputs)]
    (prn :noh number-of-hires)
    {:done "OK"}))
