(ns reports-api.time
  (:import [java.time Instant LocalDateTime ZoneId ZonedDateTime ZoneOffset]))

(def utc (ZoneId/of "UTC"))

(defn ts-to-date [ts]
  (LocalDateTime/ofInstant (Instant/ofEpochMilli ts) utc))

(defn years-from [ts years]
  (ts-to-date (+ ts (* years 31536000000))))

(defn years-from-now [years]
  (years-from (System/currentTimeMillis) years))

(defn- now []
  (.toLocalDateTime (ZonedDateTime/now utc)))

(defn format-date [date]
  (format "%d-%02d" (.getYear date) (.getMonthValue date)))

(defn date-to-ts [local-date-time]
  (.toEpochMilli (.toInstant local-date-time ZoneOffset/UTC)))

(defn date-to-month [local-date-time]
  {:year (.getYear local-date-time)
   :month (.getMonthValue local-date-time)})

(defn assert-month
  ([month] (assert-month nil month))

  ([fn-name {:keys [year month] :as m}]
   (let [label (if fn-name (str fn-name ": ") "")]
     (assert (int? year)
             (str label "Year must be a number, got " (pr-str m)))
     (assert (and (int? month) (<= 1 month 12))
             (str label "Month must be a number between 1 and 12, got " (pr-str m))))))

(defn assert-date
  ([date] (assert-date nil date))

  ([fn-name date]
   (let [label (if fn-name (str fn-name ": ") "")]
     (assert (instance? LocalDateTime date)
             (str label "Date must be an LocalDateTime, got " (pr-str date))))))

(defn next-month
  ([month] (next-month month 1))

  ([month step]
   (assert-month month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot (dec total-months) 12)
         months (inc (mod (dec total-months) 12))]
     {:year years :month months})))

(defn prev-month
  ([month] (prev-month month 1))

  ([month step]
   (assert-month month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot total-months 12)
         months (mod total-months 12)]
     {:year years :month months})))

(defn current-month []
  (date-to-month (now)))

(defn format-month [{:keys [year month]}]
  (format "%d-%02d" year month))

(defn month-to-int [{:keys [year month] :as m}]
  (assert-month m)
  (+ (* year 12) month))

(defn compare-month [m1 m2]
  (and (assert-month m1) (assert-month m2))
  (compare (month-to-int m1) (month-to-int m2)))

(defn month-to-ts [{:keys [year month] :as m}]
  (assert-month m)
  (-> (LocalDateTime/of year month 1 0 0)
      (.atZone ZoneOffset/UTC)
      (.toInstant)
      (.toEpochMilli)))
