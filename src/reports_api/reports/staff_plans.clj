(ns reports-api.reports.staff-plans
  (:require [clojure.string :as str]
            ;; [reports-api.helpers :as h]
            ;; [reports-api.time :as t]
            [reports-api.validators :as v]
            [reports-api.reports.staff-plan :as sp]))

(defn validate-inputs [inputs]
  (-> {}
      (merge (v/validate-projections-keys inputs))
      (merge {:staff (map sp/validate-inputs (or (:staff inputs) []))})))

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :clean-inputs inputs)
    ;; TODO: Fall back onto functions in staff-plan and just group them by business function.
    ;; TODO: Calculate totals.
    ))
