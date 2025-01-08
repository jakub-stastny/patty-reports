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

;;  -1 or 0 or 1 for (previous/same/following month).
;; or 3, 6, 9, 12 for last month of a quarter
;; or 1, 4, 7, 10 for month following end of a quarter
(def employer-tax-timing-opts
  {:prev-month -1 :same-month 0 :following-month 1
   :last-month-of-quarter [3 6 9 12]
   :month-following-end-of-quarter [1 4 7 10]})

(def employer-tax-timing-validator
  (v/make-validator :employer-tax-timing
                    (str "must be one of: " (pr-str employer-tax-timing-opts))
                    (fn [value]
                      (some (fn [[k v]] (when (= v value) k)) employer-tax-timing-opts))))

(defn validate-pay-change [pc]
  (let [[ts _ value] (str/split pc #"\|")
        [ts value] [(Long/parseLong ts) (Double/parseDouble value)]]
    {:effective-date (v/validate {:ts ts} :ts [v/timestamp-validator v/dt-converter])
     :new-value (v/validate {:value value} :value [v/double-validator])}))

(def benefits-payment-frequency-validator
  (v/make-validator :benefits-payment-frequency
                    "must be either a number between 1 and 12 or an array of such numbers"
                    (fn [v]
                      (or (and (int? v) (<= 1 v 12) #{v})
                          (and (vector? v) (every? int? v) (into (sorted-set) v))))))

(defn validate-inputs [inputs]
  (let [validate (fn [k validators] {k (v/validate inputs k validators)})
        validate-or-default (fn [k validators default] {k (v/validate-or-default inputs k validators default)})]

    (-> {}
        (merge (validate-or-default :projections-duration [(v/generate-range-validator 1 5)] 1))
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
        (merge (validate-or-default :benefits-payment-frequency [benefits-payment-frequency-validator] (into (sorted-set) (range 1 13))))

        (merge (validate-or-default :employer-tax-rate [(v/generate-range-validator 0 1)] 0))
        (merge (validate-or-default :employer-tax-timing [employer-tax-timing-validator] :same-month))
        (merge {:pay-changes (map validate-pay-change (or (:pay-changes inputs) []))}))))

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

(defn calculate-monthly-pay [month {:keys [work-weeks-per-year work-hours-per-week pay-structure] :as inputs}]
  (let [current-rates (calculate-current-rates month inputs)
        current-ratios (convert-rates-to-ratios current-rates)
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

(defn calculate-payroll-tax [months-till-current employer-tax-rate employer-tax-timing]
  (case employer-tax-timing
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
                                     employer-tax-timing benefits-payment-frequency] :as inputs}]
  (let [monthly-pay (* (calculate-monthly-pay month inputs) number-of-hires)
        months-till-current (conj prev-months {:month month :monthly-pay monthly-pay})

        benefits (calculate-benefits months-till-current benefits-allowance benefits-payment-frequency)
        employer-payroll-tax (calculate-payroll-tax months-till-current employer-tax-rate employer-tax-timing)

        staff-cost (+ monthly-pay benefits employer-payroll-tax)]
    {:month month :timestamp (t/month-to-ts month) :monthly-pay monthly-pay
     :benefits benefits :employer-payroll-tax employer-payroll-tax :staff-cost staff-cost}))

(defn format-for-bubble [results]
  ;; (prn :results results)
  (reduce (fn [acc {:keys [timestamp monthly-pay benefits employer-payroll-tax staff-cost]}]
            ;; TODO (key acc), pull these out of the item (dont' destructure).
            (-> acc
                (update :timestamp conj timestamp)
                (update :monthly-pay conj monthly-pay)
                (update :benefits conj benefits)
                (update :employer-payroll-tax conj employer-payroll-tax)
                (update :staff-cost conj staff-cost)))
          {:timestamp [] :monthly-pay [] :benefits [] :employer-payroll-tax [] :staff-cost []}
          results))

(defn handle [raw-inputs]
  (format-for-bubble
   (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
     (prn :clean-inputs inputs)
     (first (reduce (fn [[report month] _]
                      [(conj report (generate-report-month report month inputs)) (t/next-month month)])
                    [[] projections-start-date]
                    (repeat (* projections-duration 12) nil))))))
