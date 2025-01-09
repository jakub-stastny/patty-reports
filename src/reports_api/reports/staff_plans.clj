(ns reports-api.reports.staff-plans
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.validators :as v]
            [reports-api.reports.staff-plan :as sp]))

(defn validate-business-function [cleaned-member]
  (v/validate cleaned-member :business-function [v/string-validator])
  cleaned-member)

(defn validate-inputs [inputs]
  (-> {}
      (merge (v/validate-projections-keys inputs))
      (merge {:staff (map
                      (comp validate-business-function sp/validate-inputs)
                      (or (:staff inputs) []))})))

(defn sum-projections [p1 p2]
  (let [aggregate #(h/sum-vectors (get p1 %) (get p2 %))]
    {:monthly-pay (aggregate :monthly-pay)
     :payroll-tax (aggregate :payroll-tax)
     :benefits (aggregate :benefits)
     :staff-cost (aggregate :staff-cost)}))

(defn aggregate-by-business-function [data]
  (reduce (fn [acc {:keys [business-function projections]}]
            (let [existing-bubble-formatted-projections (get acc business-function)
                  bubble-formatted-projections (sp/add-yearly-totals (sp/format-for-bubble projections))]
              (if existing-bubble-formatted-projections
                (let [aggregated-projections
                      (sum-projections existing-bubble-formatted-projections
                                       bubble-formatted-projections)

                      aggregated-projections-with-totals
                      (merge aggregated-projections
                             {:totals (sum-projections (:totals existing-bubble-formatted-projections)
                                                       (:totals bubble-formatted-projections))})]
                  (merge acc {business-function aggregated-projections-with-totals}))
                (merge acc {business-function (dissoc bubble-formatted-projections :timestamp)}))))
          {}
          data))

;; This adds both per-item totals as well as overall totals.
(defn add-totals [results]
  (reduce (fn [acc [biz-fn projections]]
            (let [calc
                  (fn [key]
                    ;; Update per-item totals.
                    (let [existing-data (get-in acc [biz-fn key])]
                      (h/sum-vectors (or existing-data []) (get projections key))))

                  updated-acc
                  (if (get-in acc [:projections biz-fn])
                    (update acc :projections merge-with + {biz-fn projections})
                    (update acc :projections merge {biz-fn projections}))]

              (prn :acc biz-fn acc)
              (prn :updated-acc updated-acc)
              (prn :r {:totals {:monthly-pay (calc :monthly-pay)
                                :payroll-tax (calc :payroll-tax)
                                :benefits (calc :benefits)
                                :staff-cost (calc :staff-cost)}})
              (println)

              (-> updated-acc
                  (update-in [:totals :monthly-pay] h/sum-vectors (:monthly-pay projections))
                  (update-in [:totals :payroll-tax] h/sum-vectors (:payroll-tax projections))
                  (update-in [:totals :benefits] h/sum-vectors (:benefits projections))
                  (update-in [:totals :staff-cost] h/sum-vectors (:staff-cost projections)))))

          ;; TODO: :totals/:totals!!!
          {:projections {}
           :totals {:monthly-pay [] :payroll-tax [] :benefits [] :staff-cost []}}

          results))

;; TODO: Add headcuont.
(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration staff]}
        (validate-inputs raw-inputs)

        staff
        (map #(dissoc % :projections-start-date :projections-duration) staff)

        results
        (flatten (map (fn [{:keys [business-function] :as member}]
                        {:business-function business-function
                         :projections (sp/generate-projections
                                       projections-start-date
                                       projections-duration
                                       member)})
                      staff))

        timestamps (map :timestamp (:projections (first results)))
        aggregated-results (aggregate-by-business-function results)
        aggregated-results-with-totals (add-totals aggregated-results)]
    (merge {:timestamps timestamps} aggregated-results-with-totals)))
