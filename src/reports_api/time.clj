(ns reports-api.time
  "My extensions to the jakub-stastny.month library."
  (:refer-clojure :exclude [<= compare min max +])
  (:import [java.time Instant LocalDateTime ZoneId ZonedDateTime ZoneOffset])
  (:require [jakub-stastny.month]
            [jakub-stastny.extensions.define :refer [define]]
            [jakub-stastny.extensions.assertions :as jsa]
            [reports-api.helpers :as h]))

(doseq [[sym v] (ns-publics 'jakub-stastny.month)]
  (intern *ns* sym v))

(def utc (ZoneId/of "UTC"))

(defn assert-date
  ([date] (assert-date :assert-date date))

  ([fn-name date]
   (assert (instance? LocalDateTime date)
           (str (name fn-name) ": date must be an LocalDateTime, got " (pr-str date)))))

(define date-to-month [fn-name local-date-time] ;; Rename to date as in format-date?
  (assert-date fn-name local-date-time)
  {:year (.getYear local-date-time)
   :month (.getMonthValue local-date-time)})

(define ts-to-date [fn-name ts]
  (assert-timestamp fn-name ts)
  (LocalDateTime/ofInstant (Instant/ofEpochMilli ts) utc))

(define years-from [fn-name ts years]
  (assert-timestamp fn-name ts)
  (jsa/assertions fn-name years [int?] "Years must be a number")
  (ts-to-date (clojure.core/+ ts (* years 31536000000))))

(define years-from-now [fn-name years]
  (jsa/assertions fn-name years [int?] "Years must be a number")
  (years-from (System/currentTimeMillis) years))

(defn- now []
  (.toLocalDateTime (ZonedDateTime/now utc)))

(defn current-month []
  (date-to-month (now)))

(define format-date [fn-name date]
  (assert-date fn-name date)
  (format "%d-%02d" (.getYear date) (.getMonthValue date)))

(define date-to-ts [fn-name local-date-time] ;; Rename to date as in format-date?
  (assert-date fn-name local-date-time)
  (.toEpochMilli (.toInstant local-date-time ZoneOffset/UTC)))

(define month-to-ts [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (-> (LocalDateTime/of year month 1 0 0)
      (.atZone ZoneOffset/UTC)
      (.toInstant)
      (.toEpochMilli)))

(define month-to-date [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (LocalDateTime/of year month 1 0 0 0 0))
