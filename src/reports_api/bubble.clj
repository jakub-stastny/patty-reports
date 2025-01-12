(ns reports-api.bubble
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]))

(defn format-for-bubble-one [results]
  (reduce (fn [acc {:keys [timestamp monthly-pay benefits payroll-tax staff-cost]}]
            ;; TODO (key acc), pull these out of the item (dont' destructure).
            (-> acc
                (update :timestamp conj timestamp)
                (update :monthly-pay conj monthly-pay)
                (update :benefits conj benefits)
                (update :payroll-tax conj payroll-tax)
                (update :staff-cost conj staff-cost)))
          {:timestamp [] :monthly-pay [] :benefits [] :payroll-tax [] :staff-cost []}
          results))
