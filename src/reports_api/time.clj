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

(h/defn-pass-name ts-to-date [fn-name ts]
  (assert-timestamp fn-name ts)
  (LocalDateTime/ofInstant (Instant/ofEpochMilli ts) utc))

(h/defn-pass-name years-from [fn-name ts years]
  (assert-timestamp fn-name ts)
  (h/assertions fn-name years [int?] "Years must be a number")
  (ts-to-date (+ ts (* years 31536000000))))

(h/defn-pass-name years-from-now [fn-name years]
  (h/assertions fn-name years [int?] "Years must be a number")
  (years-from (System/currentTimeMillis) years))

(defn- now []
  (.toLocalDateTime (ZonedDateTime/now utc)))

(h/defn-pass-name format-date [fn-name date]
  (assert-date fn-name date)
  (format "%d-%02d" (.getYear date) (.getMonthValue date)))

(h/defn-pass-name date-to-ts [fn-name local-date-time] ;; Rename to date as in format-date?
  (assert-date fn-name local-date-time)
  (.toEpochMilli (.toInstant local-date-time ZoneOffset/UTC)))

(h/defn-pass-name date-to-month [fn-name local-date-time] ;; Rename to date as in format-date?
  (assert-date fn-name local-date-time)
  {:year (.getYear local-date-time)
   :month (.getMonthValue local-date-time)})

(h/defn-pass-name next-month
  ([_ month] (next-month month 1))

  ([fn-name month step]
   (assert-month fn-name month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot (dec total-months) 12)
         months (inc (mod (dec total-months) 12))]
     {:year years :month months})))

;; TODO: Will the pass macro work here?
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

(h/defn-pass-name format-month [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (format "%d-%02d" year month))

(h/defn-pass-name month-to-int [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (+ (* year 12) month))

(h/defn-pass-name compare-month [fn-name m1 m2]
  (and (assert-month fn-name m1)
       (assert-month fn-name m2))
  (compare (month-to-int m1) (month-to-int m2)))

(h/defn-pass-name month-to-ts [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (-> (LocalDateTime/of year month 1 0 0)
      (.atZone ZoneOffset/UTC)
      (.toInstant)
      (.toEpochMilli)))
