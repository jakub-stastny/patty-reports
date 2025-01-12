;; TODO: Make generic.
;; This has been copied from the staff-plan without any changes.

(ns reports-api.pro-rata-engine
  (:require [clojure.string :as str]
            [reports-api.time :as t]))

(defn calculate-pro-rata-base-pay [month base-pay rate current-month-pay-changes employment-start-date employment-end-date]
  (let [rates
        (map (fn [pc]
               {:since (.getDayOfMonth (:effective-date pc)) :rate (:new-value pc)})
             current-month-pay-changes)

        rates
        (if (= (t/format-month month) (t/format-date employment-start-date))
          (conj rates {:since (.getDayOfMonth employment-start-date) :rate base-pay}) rates)

        rates
        (if (= (t/format-month month) (t/format-date employment-end-date))
          (conj rates {:since (.getDayOfMonth employment-end-date) :rate 0}) rates)]

    (if (some #(= 1 (:since %)) rates)
      (into [] rates)
      (into [{:since 1 :rate rate}] rates))))

(defn find-last-pay-change-before-current-month [month pay-changes]
  (let [prev-changes
        (filter #(= 1 (t/compare-month month (t/date-to-month (:effective-date %)))) pay-changes)]
    (last (sort-by :effective-date prev-changes))))

(defn filter-current-month-pay-changes [month pay-changes]
  (filter #(= 0 (t/compare-month month (t/date-to-month (:effective-date %)))) pay-changes))

(defn calculate-current-rates [month {:keys [base-pay pay-changes employment-start-date employment-end-date]}]
  (let [last-pay-change-before-current-month
        (find-last-pay-change-before-current-month month pay-changes)
        dt-to-int #(t/month-to-int (t/date-to-month %))

        working-on-the-1st
        (< (t/date-to-ts employment-start-date)
           (t/month-to-ts month)
           (t/date-to-ts employment-end-date))

        work-status-changes-this-month
        (or (= (t/format-month month) (t/format-date employment-start-date))
            (= (t/format-month month) (t/format-date employment-end-date)))

        ;; What pay-rate is vigent on the 1st.
        current-base-pay-rate
        (cond
          (not working-on-the-1st) 0

          (and last-pay-change-before-current-month)
          (:new-value last-pay-change-before-current-month)

          :else base-pay)

        current-month-pay-changes
        (filter-current-month-pay-changes month pay-changes)]

    (if (and (empty? current-month-pay-changes)
             (not work-status-changes-this-month))
      [{:since 1 :rate current-base-pay-rate}]
      (calculate-pro-rata-base-pay month base-pay current-base-pay-rate current-month-pay-changes employment-start-date employment-end-date))))

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
