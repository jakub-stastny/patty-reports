(ns reports-api.reports.staff-plans
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.validators :as v]
            [reports-api.totals :as tot]
            [reports-api.bubble :as b]
            [reports-api.reports.staff-plan :as sp]))

(defn validator-fn [member]
  (try (sp/validate-inputs member v/undefined)
       (catch clojure.lang.ExceptionInfo error
         {:error (:reason (ex-data error))})))

(defn validate-staff [state inputs]
  (v/validate-items state inputs :staff validator-fn))

(defn validate-inputs [inputs]
  (v/ensure-valid
   (-> {:errors {} :data {}}
       (v/validate-projections-keys inputs)
       (validate-staff inputs))))

(defn sum-projections [p1 p2]
  (let [aggregate #(h/sum-vectors (get p1 %) (get p2 %))]
    {:monthly-pay (aggregate :monthly-pay)
     :payroll-tax (aggregate :payroll-tax)
     :benefits (aggregate :benefits)
     :staff-cost (aggregate :staff-cost)}))

(defn aggregate-by-business-function [data]
  (reduce (fn [acc {:keys [business-function projections]}]
            (let [existing-bubble-formatted-projections (get acc business-function)
                  bubble-formatted-projections (tot/add-yearly-totals-one (b/format-for-bubble-one projections))]
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

;; TODO: Add headcuont.
(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration staff] :as inputs}
        (validate-inputs raw-inputs)

        _ (prn :clean-inputs inputs)

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
        aggregated-results-with-totals (tot/add-totals-all aggregated-results)]
    (merge {:timestamps timestamps} aggregated-results-with-totals)))
