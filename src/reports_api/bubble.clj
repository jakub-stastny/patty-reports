(ns reports-api.bubble
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]))

(def ignored-keys #{:month :relative-month :being-sold :sales-growth-rate
                    :seasonal-adjustment-rate :pro-rata-factor})

(defn format-for-bubble-one [results keys]
  (reduce (fn [acc report-month-data]
            (reduce (fn [acc key]
                      (if (ignored-keys key)
                        acc
                        (update acc key conj (h/round (h/get! report-month-data key)))))
                    acc keys))
          (zipmap keys (repeat []))
          results))
