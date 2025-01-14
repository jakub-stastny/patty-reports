(ns reports-api.time
  (:import [java.time Instant LocalDateTime ZoneId ZonedDateTime ZoneOffset])
  (:require [reports-api.helpers :as h]))

(def utc (ZoneId/of "UTC"))

(defn assert-timestamp
  ([ts]
   (assert-timestamp :assert-timestamp ts))
  ([fn-name ts]
   (h/assertions fn-name ts [int?] "Timestamp must be a number")))

(defn assert-month
  ([month] (assert-month :assert-month month))

  ([fn-name {:keys [year month] :as m}]
   (assert (int? year)
           (str (name fn-name) ": year must be a number, got " (pr-str m)))
   (assert (and (int? month) (<= 1 month 12))
           (str (name fn-name) ": month must be a number between 1 and 12, got " (pr-str m)))))

(defn assert-date
  ([date] (assert-date :assert-date date))

  ([fn-name date]
   (assert (instance? LocalDateTime date)
           (str (name fn-name) ": date must be an LocalDateTime, got " (pr-str date)))))

(defn ts-to-date [ts]
  (assert-timestamp :ts-to-date ts)
  (LocalDateTime/ofInstant (Instant/ofEpochMilli ts) utc))

(defn years-from [ts years]
  (assert-timestamp :years-from ts)
  (h/assertions :years-from years [int?] "Years must be a number")
  (ts-to-date (+ ts (* years 31536000000))))

(defn years-from-now [years]
  (h/assertions :years-from years [int?] "Years must be a number")
  (years-from (System/currentTimeMillis) years))

(defn- now []
  (.toLocalDateTime (ZonedDateTime/now utc)))

(defn format-date [date]
  (assert-date :format-date date)
  (format "%d-%02d" (.getYear date) (.getMonthValue date)))

(defn date-to-ts [local-date-time] ;; Rename to date as in format-date?
  (assert-date :date-to-ts local-date-time)
  (.toEpochMilli (.toInstant local-date-time ZoneOffset/UTC)))

(defn date-to-month [local-date-time] ;; Rename to date as in format-date?
  (assert-date :date-to-month local-date-time)
  {:year (.getYear local-date-time)
   :month (.getMonthValue local-date-time)})

(defn next-month
  ([month] (next-month month 1))

  ([month step]
   (assert-month :next-month month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot (dec total-months) 12)
         months (inc (mod (dec total-months) 12))]
     {:year years :month months})))

(defn prev-month
  ([month] (prev-month month 1))

  ([month step]
   (assert-month :prev-month month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot total-months 12)
         months (mod total-months 12)]
     {:year years :month months})))

(defn current-month []
  (date-to-month (now)))

(defn format-month [{:keys [year month] :as m}]
  (assert-month :format-month m)
  (format "%d-%02d" year month))

(defn month-to-int [{:keys [year month] :as m}]
  (assert-month :month-to-int m)
  (+ (* year 12) month))

(defn compare-month [m1 m2]
  (and (assert-month :compare-month m1)
       (assert-month :compare-month m2))
  (compare (month-to-int m1) (month-to-int m2)))

(defn month-to-ts [{:keys [year month] :as m}]
  (assert-month :month-to-ts m)
  (-> (LocalDateTime/of year month 1 0 0)
      (.atZone ZoneOffset/UTC)
      (.toInstant)
      (.toEpochMilli)))
