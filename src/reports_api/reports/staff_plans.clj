(ns reports-api.reports.staff-plans
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            ;; [reports-api.time :as t]
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
  (let [aggregate #(h/sum-vectors (mapv % p1) (mapv % p2))]
    {:monthly-pay (aggregate :monthly-pay)
     :employer-payroll-tax (aggregate :employer-payroll-tax)
     :benefits (aggregate :benefits)
     :staff-cost (aggregate :staff-cost)}))

(defn aggregate-by-business-function [data]
  (reduce (fn [acc {:keys [business-function projections]}]
            (prn :acc business-function acc (get acc business-function))
            (println)
            (if (get acc business-function)
              (let [existing-bubble-formatted-projections (get acc business-function)
                    bubble-formatted-projections (sp/format-for-bubble projections)]
                (merge acc {business-function (sum-projections existing-bubble-formatted-projections
                                                               bubble-formatted-projections)}))

              (merge acc {business-function (sp/format-for-bubble projections)})))

          {:timestamp (:timestamp (first data))}
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

        aggregated-results (aggregate-by-business-function results)]
    aggregated-results))
