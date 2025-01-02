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

;; (defn date-to-ts [local-date-time]
;;   (.toEpochMilli (.toInstant local-date-time ZoneOffset/UTC)))

;; CHANGE: convert everything into {:month, :year} map.
(defn extract-year-and-month [local-date-time]
  {:year (.getYear local-date-time)
   :month (.getMonthValue local-date-time)})

(defn next-month
  ([month] (next-month month 1))

  ([month step]
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot total-months 12)
         months (mod total-months 12)]
     {:year years :month months})))

(defn prev-month
  ([month] (prev-month month 1))

  ([month step]
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot total-months 12)
         months (mod total-months 12)]
     {:year years :month months})))

(defn current-month []
  (extract-year-and-month (now)))

(defn format-month [{:keys [year month]}]
  (format "%d-%02d" year month))

(defn compare-month [m1 m2]
  (compare (+ (* (:year m1) 12) (:month m1))
           (+ (* (:year m2) 12) (:month m2))))

(defn month-to-ts [{:keys [year month]}]
  (-> (LocalDateTime/of year month 1 0 0)
      (.atZone ZoneOffset/UTC)
      (.toInstant)
      (.toEpochMilli)))
