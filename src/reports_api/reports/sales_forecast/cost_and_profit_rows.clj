(ns reports-api.reports.sales-forecast.cost-and-profit-rows
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
            [reports-api.fsl :refer :all]
            [reports-api.time :as t]))

(property :cost-of-sales (* (:units-sold rs) (:cost rs)))

(property :bad-debt-provision
          (* -1 (:bad-debt-provision in) (:total-revenue rs)))

(property :gross-profit
          (+ (:net-revenue rs) (:cost rs) (:bad-debt-provision rs)))

(defn process [prev-months month inputs results]
  (h/calc-props
   'reports-api.reports.sales-forecast.cost-and-profit-rows
   [:cost-of-sales :bad-debt-provision :gross-profit]
   (merge results {:cost 1 :net-revenue 200}) prev-months month inputs))
