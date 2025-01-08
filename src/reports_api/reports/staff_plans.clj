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
                  bubble-formatted-projections (sp/format-for-bubble projections)]
              (if (get acc business-function)
                (merge acc {business-function (sum-projections existing-bubble-formatted-projections
                                                               bubble-formatted-projections)})
                (merge acc {business-function (sp/format-for-bubble projections)}))))
          {}
          data))

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

        aggregated-results (aggregate-by-business-function results)]
    (merge {:timestamps timestamps} aggregated-results)))
