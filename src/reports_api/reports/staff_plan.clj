(ns reports-api.reports.staff-plan)

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

(defn handle-staff-plan [inputs]
  (prn :inputs inputs)
  {:done "OK"})
