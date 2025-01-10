(ns reports-api.validators
  (:require [clojure.string :as str]
            [reports-api.time :as t]))

(defn make-validator [type message validator]
  {:type type :message message :validator validator})

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

(def boolean-validator
  (make-validator :boolean "must be a boolean"
                 #(and (boolean? %) %)))

;; current-ms (System/currentTimeMillis)
(def timestamp-validator
  (make-validator :timestamp
                  "must be a UNIX timestamp between 0 (year 1970) and 4102444800000 (year 2100)"
                  #(and (number? %) (<= 0 % 4102444800000) %)))

(def single-or-multiple-months-validator
  (make-validator :single-or-multiple-months
                  "must be either a number between 1 and 12 or an array of such numbers"
                  (fn [v]
                    (or (and (int? v) (<= 1 v 12) #{v})
                        (and (vector? v) (every? int? v) (into (sorted-set) v))))))

(def single-or-multiple-months-or-weekly-or-daily-validator
  (make-validator :single-or-multiple-months-or-weekly-or-daily
                  "must be either a number between 1 and 12 or an array of such numbers or 365 for daily or 52 for weekly"
                  (fn [v]
                    (or (and (int? v) (<= 1 v 12) #{v})
                        (and (vector? v) (every? int? v) (into (sorted-set) v))
                        (and (= 365 v) :daily)
                        (and (= 52 v) :weekly)))))

(def optional-single-or-multiple-months-validator
  (make-validator :optional-months
                 "must be either nil, a number between 1 and 12, or an array of such numbers"
                 (fn [v]
                   (when-let [result
                             (or (nil? v)
                                 (and (int? v) (<= 1 v 12) #{v})
                                 (and (sequential? v)
                                      (every? #(and (int? %) (<= 1 % 12)) v)
                                      (into (sorted-set) v)))]
                     result))))

(defn generate-range-validator [min max]
  (make-validator (keyword (str min "-to-" max))
                  (str "must be between " min " and " max)
                  #(and (<= min % max) %)))

(def dt-converter
  (make-validator :dt-converter "" #(t/ts-to-date %)))

(def month-converter
  (make-validator :month-converter "" #(t/date-to-month (t/ts-to-date %))))

(defn generate-options-validator [field-name options-map]
  (make-validator (keyword field-name)
                 (str "must be one of: " (str/join "," (keys options-map)))
                 (fn [v]
                   (when-let [matched-key (some #(and (= % v) %) (keys options-map))]
                     (get options-map matched-key)))))

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

(defn validate-projections-keys [inputs]
  (let [validate-or-default
        (fn [k validators default]
          {k (validate-or-default inputs k validators default)})]
    (->
     (merge (validate-or-default :projections-duration [(generate-range-validator 1 5)] 1))
     (merge (validate-or-default :projections-start-date [timestamp-validator month-converter] (t/current-month))))))
