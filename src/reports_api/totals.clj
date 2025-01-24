(ns reports-api.totals
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]))

;; One person working for 12 months non-accumulative, unlike say tax,
;; which adds up. Hence we need to divide month-person by 12.
(def filters {:headcount (fn [years-vals] (map #(/ % 12) years-vals))})

(defn- generate-reduce-fn [results]
  (fn [acc key]
    (let [vals (h/get! results key)
          years (partition 12 vals)
          value (map #(reduce + %) years)

          processed-value
          (if-let [filter (get filters key)]
            (filter value) value)]
      (if (some nil? vals)
        (throw (ex-info "Value was nil" {:key key :vals vals}))
        (update acc :totals merge {key processed-value})))))

(defn add-yearly-totals-one [results keys]
  (merge results
         (reduce (generate-reduce-fn results) {:totals {}} keys)))

;; This adds both per-item totals as well as overall totals.
(defn add-totals-all [results keys]
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

              (reduce (fn [acc key]
                        (update-in acc [:totals key] h/sum-vectors (get projections key)))
                      updated-acc keys)))

          ;; TODO: :totals/:totals!!!
          {:projections {} :totals (zipmap keys (repeat []))}

          results))
