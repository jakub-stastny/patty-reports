(ns reports-api.reports.staff-plans
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.validators :as v]
            [reports-api.reports.staff-plan :as sp]))

(defn validate-inputs [inputs]
  inputs)

(defn handle [raw-inputs]
  (let [{:keys [projections-start-date projections-duration] :as inputs} (validate-inputs raw-inputs)]
    (prn :clean-inputs inputs)))
