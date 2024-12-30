(ns reports-api.reports.staff-plan
  (:require [reports-api.helpers :as h]))

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

(def number-validator
  {:type :number :validator #(and (number? %) (not (neg? %))) :message "must be a positive number"})

(defn throw-validation-error [m k v]
  (throw (ex-info "Validation error" {:type :validation-error :reason m :key k :value v})))

(defn validate [m k validators]
  (let [v (get m k)]
    (doseq [{:keys [type validator message]} validators]
      (when-not (validator v)
        (println (str "Validator " type " failed for " k " = " (pr-str v)))
        (throw-validation-error message k v)))))

(def defaults {:number-of-hires 1})

(defn validate-inputs [raw-inputs]
  (let [inputs (h/transform-keys-to-kebab-case raw-inputs)
        ;; {:keys
        ;;  [projection-start-date number-of-hires employment-start-date
        ;;   employment-end-date pay-structure work-hours-per-week work-hours-per-year
        ;;   base-pay benefits-allowance-%] :as inputs} inputs
        ]

    (if (:contains? inputs :projections-duration)
      (validate inputs :projections-duration [number-validator {:type :1-to-5 :validator #(<= 1 % 5) :message "must be between 1 and 5"}]))

    (if (:contains? inputs :number-of-hires)
      (validate inputs :number-of-hires [number-validator]))

    (merge defaults inputs)))

(defn handle [raw-inputs]
  (let [{:keys [number-of-hires] :as inputs} (validate-inputs raw-inputs)]
    (prn :noh number-of-hires)
    {:done "OK"}))
