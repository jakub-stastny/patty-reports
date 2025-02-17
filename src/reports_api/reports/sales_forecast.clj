(ns reports-api.reports.sales-forecast
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [reports-api.helpers :as h]
            [reports-api.time :as t]
            [reports-api.reports.sales-forecast.validators :as v]
            [reports-api.xhelpers :as xh]
            [reports-api.bubble :as b]
            [reports-api.totals :as tot]

            [reports-api.reports.sales-forecast.helper-rows]
            [reports-api.reports.sales-forecast.customer-rows]
            [reports-api.reports.sales-forecast.revenue-rows]
            [reports-api.reports.sales-forecast.vat-rows]
            [reports-api.reports.sales-forecast.cost-and-profit-rows]
            [reports-api.reports.sales-forecast.cash-flow-rows]))

(def ^:private row-namespaces
  ['reports-api.reports.sales-forecast.helper-rows
   'reports-api.reports.sales-forecast.customer-rows
   'reports-api.reports.sales-forecast.revenue-rows
   'reports-api.reports.sales-forecast.vat-rows
   'reports-api.reports.sales-forecast.cost-and-profit-rows
   'reports-api.reports.sales-forecast.cash-flow-rows])

(defn generate-report-month [prev-months month inputs]
  (let [processing-fns (keep #(ns-resolve % 'process) row-namespaces)]
    (reduce (fn [results f]
              (merge results (f prev-months month inputs results)))
            {} processing-fns)))

(def row-ns-props
  (reduce (fn [acc namespace]
            (let [all-fns (map str (keys (ns-publics namespace)))

                  calculate-fns
                  (filter #(str/starts-with? % "calculate-") all-fns)

                  properties
                  (map #(str/replace-first % #"^calculate-" "") calculate-fns)]
              (merge acc {(keyword namespace) (map keyword properties)})))
          {}
          row-namespaces))

(def all-keys (set (apply concat (vals row-ns-props))))

(def ignored-keys #{:month :relative-month :being-sold ; These are definitely not useful at all.
                    :sales-growth-rate :seasonal-adjustment-rate :pro-rata-factor ; We could include these, but remove them from totals.

                    ;; Only temporarily disabled.
                    :sales-revenue-due :bad-debts :sales-revenue-received

                    :cost-of-sales :bad-debt-provision :gross-profit

                    :vat-in :vat-out

                    ;; :non-seasonal-revenue-target :required-customers :customer-base :domestic-sales
                    ;; :eu-sales :rest-of-world-sales :returns-and-refunds :net-sales-revenue :units-sold
                    })

(defn handle [raw-inputs]
  (let [inputs (v/validate-inputs raw-inputs)
        _ (prn :inputs inputs)
        projections (xh/generate-projections inputs generate-report-month)]
    (println) (prn :projections projections)
    (tot/add-yearly-totals-one
     (b/format-for-bubble-one projections (set/difference all-keys ignored-keys))
     (set/difference all-keys ignored-keys))))
