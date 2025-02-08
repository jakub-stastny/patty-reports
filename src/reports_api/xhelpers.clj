(ns reports-api.xhelpers
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [jakub-stastny.extensions.define :refer [define]]
            [jakub-stastny.extensions.assertions :as jsa]
            [reports-api.time :as t]))

(define projection-months [fn-name {:keys [projections-start-date projections-duration]}]
  (t/assert-month fn-name projections-start-date)
  (jsa/assertions fn-name projections-duration [int?] "must be int")

  (let [total-months (* projections-duration 12)]
    (take total-months (iterate t/next-month projections-start-date))))

;; Month carries relative month thus:
;; {:year 2025 :month 1 :relative {:year 0 :month 1}}}
(defn generate-projections [inputs generate-report-month-fn]
  (let [months (projection-months inputs)]
    (:report-acc
     (reduce
      (fn [{:keys [report-acc relative-month] :as x} month]
        (let [updated-month (merge month {:relative relative-month})
              current-month-report
              (h/run-custom-fn generate-report-month-fn report-acc updated-month inputs)]
          {:relative-month (t/next-month relative-month)
           :report-acc (conj report-acc current-month-report)}))

      {:relative-month {:year 0 :month 1} :report-acc []}
      months))))
