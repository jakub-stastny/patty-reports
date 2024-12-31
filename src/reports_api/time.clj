(ns reports-api.time
  (:import [java.time Instant LocalDateTime ZoneId ZonedDateTime]))

(def utc (ZoneId/of "UTC"))

(defn ts-to-date [ts]
  (LocalDateTime/ofInstant (Instant/ofEpochMilli ts) utc))

(defn now []
  (.toLocalDateTime (ZonedDateTime/now utc)))

(defn years-from-now [years]
  (ts-to-date (+ (System/currentTimeMillis) (* years 31536000000))))
