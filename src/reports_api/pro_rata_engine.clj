(ns reports-api.pro-rata-engine
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(defn- assert-change [{:keys [effective-date new-value]}]
  (t/assert-date effective-date)
  (h/assertions :assert-change new-value [number?] "new-value must be a number"))

(defn- assert-changes [changes]
  (h/assertions :assert-changes changes [sequential?] "changes must be a sequential")
  (doseq [change changes] (assert-change change)))

;; Staff plan: employment-start/end-date, base-pay, pay-changes
;; Sales forecast: sales-start/end-date, selling-price, selling-price-changes
(defn- calculate-pro-rata-initial-rate [month initial-rate current-month-rate current-month-changes start-date end-date]
  (t/assert-month :calculate-pro-rata-initial-rate month)
  (h/assertions :calculate-pro-rata-initial-rate initial-rate [number?]
                "initial-rate must be a positive number")
  (h/assertions :calculate-pro-rata-initial-rate current-month-rate [number?]
                "current-month-rate must be a positive number")
  (assert-changes current-month-changes)
  (and (t/assert-date :calculate-pro-rata-initial-rate start-date)
       (t/assert-date :calculate-pro-rata-initial-rate end-date))

  (let [rates
        (map (fn [pc]
               {:since (.getDayOfMonth (:effective-date pc)) :rate (:new-value pc)})
             current-month-changes)

        rates
        (if (= (t/format-month month) (t/format-date start-date))
          (conj rates {:since (.getDayOfMonth start-date) :rate initial-rate}) rates)

        rates
        (if (= (t/format-month month) (t/format-date end-date))
          (conj rates {:since (.getDayOfMonth end-date) :rate 0}) rates)]

    (if (some #(= 1 (:since %)) rates)
      (into [] rates)
      (into [{:since 1 :rate current-month-rate}] rates))))

(defn- find-last-pay-change-before-current-month [month all-changes]
  (let [prev-changes
        (filter #(= 1 (t/compare-month month (t/date-to-month (:effective-date %)))) all-changes)]
    (last (sort-by :effective-date prev-changes))))

(defn- filter-changes [month all-changes]
  (filter #(= 0 (t/compare-month month (t/date-to-month (:effective-date %)))) all-changes))

(defn calculate-current-rates [month initial-rate all-changes start-date end-date]
  (t/assert-month :calculate-current-rates month)
  (h/assertions :calculate-current-rates initial-rate [number? pos?]
                "initial-rate must be a positive number")
  (assert-changes all-changes)
  (and (t/assert-date start-date) (t/assert-date end-date))

  (let [last-pay-change-before-current-month
        (find-last-pay-change-before-current-month month all-changes)
        dt-to-int #(t/month-to-int (t/date-to-month %))

        working-on-the-1st
        (< (t/date-to-ts start-date)
           (t/month-to-ts month)
           (t/date-to-ts end-date))

        work-status-changes-this-month
        (or (= (t/format-month month) (t/format-date start-date))
            (= (t/format-month month) (t/format-date end-date)))

        ;; What rate change is vigent on the 1st.
        current-month-rate
        (cond
          (not working-on-the-1st) 0

          (and last-pay-change-before-current-month)
          (:new-value last-pay-change-before-current-month)

          :else initial-rate)

        all-changes
        (filter-changes month all-changes)]

    (if (and (empty? all-changes)
             (not work-status-changes-this-month))
      [{:since 1 :rate current-month-rate}]
      (calculate-pro-rata-initial-rate
       month initial-rate current-month-rate all-changes start-date end-date))))

(defn calculate-pro-rata-factor [month start-date end-date]
  (let [start-month (t/date-to-month start-date)
        end-month   (t/date-to-month end-date)
        first-day-of-month 1
        last-day-of-month 30
        month-comparison-start (t/compare-month month start-month)
        month-comparison-end   (t/compare-month month end-month)]

    (cond
      ;; Case 1: Month is completely outside the range
      (or (< month-comparison-start 0)
          (> month-comparison-end 0))
      [{:since first-day-of-month :rate 0}]

      ;; Case 2: Whole month is within the range
      (and (= month-comparison-start 0)
           (= month-comparison-end 0))
      [{:since first-day-of-month :rate 1}]

      ;; Case 3: Partial month
      :else
      (let [start-day (if (= month-comparison-start 0) ;; Start date within the month
                        (.getDayOfMonth start-date)
                        first-day-of-month)
            end-day   (if (= month-comparison-end 0)   ;; End date within the month
                        (.getDayOfMonth end-date)
                        last-day-of-month)]
        [{:since start-day :rate 1}
         {:since (inc end-day) :rate 0}]))))

;; Converts: [{:since 1, :rate 90} {:since 13, :rate 100}]
;; to:       [{:days 12 :rate 90}  {:days 18 :rate 100}]
(defn convert-rates-to-ratios [rates]
  (h/assertions :convert-rates-to-ratios rates [sequential?] "Must be a sequential")
  (h/assertions :convert-rates-to-ratios rates
                [(fn [changes] (every? #(and (number? (:since %)) (number? (:rate %))) changes))]
                "must contain maps with :since and :rate, both being numbers")

  (let [days-in-month 30]
    (->> (partition 2 1 (concat rates [nil]))
         (map (fn [[current next]]
                (if current
                  {:days (if next
                           (- (:since next) (:since current))
                           (- days-in-month (:since current) -1))
                   :rate (:rate current)})))
         (remove nil?))))

(defn pro-rata-factor [rates]
  (let [ratios (convert-rates-to-ratios rates)
        total-days (reduce + (map :days ratios))
        weighted-sum (reduce + (map #(* (:days %) (:rate %)) ratios))]
    (/ weighted-sum total-days)))
