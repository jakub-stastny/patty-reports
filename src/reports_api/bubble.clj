(ns reports-api.bubble
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]))

(defn format-for-bubble-one [results keys]
  (reduce (fn [acc report-month-data]
            (reduce (fn [acc key]
                      (update acc key conj (h/round (h/get! report-month-data key))))
                    acc keys))
          (zipmap keys (repeat []))
          results))
