(ns reports-api.validators
  (:require [reports-api.time :as t]))

(defn make-validator [type message validator]
  {:type type :validator validator :message message})

(def number-validator
  (make-validator :number "must be 0 or larger"
                  #(and (number? %) (not (neg? %)) %)))

(def double-validator
  (make-validator :double "must be 0.0 or larger"
                  #(and (double? %) (not (neg? %)) %)))

(def positive-number-validator
  (make-validator :positive-number "must be a positive number"
                  #(and (number? %) (pos? %) %)))

(def string-validator
  (make-validator :string "must be a string"
                  #(and (string? %) %)))

;; current-ms (System/currentTimeMillis)
(def timestamp-validator
  (make-validator :timestamp
                  "must be a UNIX timestamp between 0 (year 1970) and 4102444800000 (year 2100)"
                  #(and (number? %) (<= 0 % 4102444800000) %)))

(defn generate-range-validator [min max]
  (make-validator (keyword (str min "-to-" max))
                  (str "must be between " min " and " max)
                  #(and (<= min % max) %)))

(def dt-converter
  (make-validator :dt-converter "" #(t/ts-to-date %)))

(def month-converter
  (make-validator :month-converter "" #(t/date-to-month (t/ts-to-date %))))

(defn- throw-validation-error [m k v]
  (throw (ex-info "Validation error" {:type :validation-error :reason m :key k :value v})))

(defn validate [m k validators]
  (let [initial-value (get m k)]
    (reduce
     (fn [current-value {:keys [type validator message]}]
       (if-let [validation-result (validator current-value)]
         validation-result
         (do
           (println (str "Validator " type " failed for " k " = " (pr-str current-value)))
           (throw-validation-error message k current-value))))
     initial-value
     validators)))

(defn validate-or-default [m k validators default-value]
  (if (contains? m k) (validate m k validators) default-value))
