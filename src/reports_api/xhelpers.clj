(ns reports-api.xhelpers
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.time :as t]))

(defn projection-months [{:keys [projections-start-date projections-duration]}]
  (t/assert-month :projection-months projections-start-date)
  (h/assertions :projection-months projections-duration [int?] "must be int")

  (let [total-months (* projections-duration 12)]
    (take total-months (iterate t/next-month projections-start-date))))

(defn generate-projections [inputs generate-report-month-fn]
  (let [months (projection-months inputs)]
    (reduce
     (fn [report-acc month]
       (let [current-month-report (generate-report-month-fn report-acc month inputs)]
         (conj report-acc current-month-report)))
     [] months)))
