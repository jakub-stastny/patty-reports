(ns reports-api.reports.sales-forecast
  (:require [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]))

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]
    (-> {}
        (merge (v/validate-projections-keys inputs))
        (merge (validate :base-revenue [v/positive-number-validator]))
        (merge (validate-or-default :growth-rate [(v/generate-range-validator 0 1)] 0)))))

(defn calculate-monthly-revenue [month {:keys [base-revenue growth-rate]} start-month]
  (let [months-elapsed (t/months-between start-month month)
        monthly-growth-rate (/ growth-rate 12)]
    (int (* base-revenue (Math/pow (+ 1 monthly-growth-rate) months-elapsed)))))

(defn generate-report-month [prev-months month inputs start-month]
  (let [monthly-revenue (calculate-monthly-revenue month inputs start-month)]
    {:month month 
     :timestamp (t/month-to-ts month)
     :revenue monthly-revenue}))

(defn format-for-bubble [results]
  (reduce (fn [acc {:keys [timestamp revenue]}]
            (-> acc
                (update :timestamp conj timestamp)
                (update :revenue conj revenue)))
          {:timestamp [] :revenue []}
          results))

(defn generate-projections [projections-start-date projections-duration inputs]
  (first (reduce (fn [[report month] _]
                   [(conj report (generate-report-month report month inputs projections-start-date)) 
                    (t/next-month month)])
                 [[] projections-start-date]
                 (repeat (* projections-duration 12) nil))))

(defn add-yearly-totals [results]
  (let [sum-vals (fn [key]
                   (let [vals (get results key)
                         years (partition 12 vals)]
                     (map #(reduce + %) years)))]
    (merge results
           {:totals
            {:revenue (sum-vals :revenue)}})))

(defn handle [raw-inputs]
  (add-yearly-totals
   (format-for-bubble
    (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
      (generate-projections projections-start-date projections-duration inputs)))))