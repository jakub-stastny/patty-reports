(ns reports-api.totals
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]))

(defn add-yearly-totals-one [results]
  (let [sum-vals
        (fn [key]
          (let [vals (get results key)
                years (partition 12 vals)]
            (map #(reduce + %) years)))]
    (merge results
           {:totals
            {:monthly-pay (sum-vals :monthly-pay)
             :benefits (sum-vals :benefits)
             :payroll-tax (sum-vals :payroll-tax)
             :staff-cost (sum-vals :staff-cost)}})))

;; This adds both per-item totals as well as overall totals.
(defn add-totals-all [results]
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
