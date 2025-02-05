(ns reports-api.validators
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]
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

(defn- num-or-vec-of-nums [v]
  (or (and (int? v) (<= 1 v 12) #{v})
      (and (vector? v) (every? int? v) (into (sorted-set) v))))

(def single-or-multiple-months-validator
  (make-validator :single-or-multiple-months
                  "must be either a number between 1 and 12 or an array of such numbers"
                  num-or-vec-of-nums))

(def single-or-multiple-months-or-weekly-or-daily-validator
  (make-validator :single-or-multiple-months-or-weekly-or-daily
                  "must be either a number between 1 and 12 or an array of such numbers or 365 for daily, 52 for weekly or 26 for fortnightly"
                  (fn [v]
                    (or (num-or-vec-of-nums v)
                        (and (= 365 v) :daily)
                        (and (= 52 v) :weekly)
                        (and (= 26 v) :fortnightly)))))

;;  -1 or 0 or 1 for (previous/same/following month).
;; or 3, 6, 9, 12 for last month of a quarter
;; or 1, 4, 7, 10 for month following end of a quarter
(def month-timing-opts
  {:prev-month -1 :same-month 0 :following-month 1
   :last-month-of-quarter [3 6 9 12]
   :month-following-end-of-quarter [1 4 7 10]})

(def month-timing-validator
  (make-validator :month-timing
                  (str "must be one of: " (pr-str month-timing-opts))
                  (fn [value]
                    (some (fn [[k v]] (when (= v value) k)) month-timing-opts))))

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

(h/defn-pass-name generate-options-validator [fn-name field-name options-map]
  (h/assertions fn-name field-name [keyword?] "field-name must be a keyword")
  (h/assertions fn-name options-map [map?] "options-map must be a map")

  (make-validator field-name
                 (str "must be one of: " (str/join ", " (keys options-map)))
                 (fn [v]
                   (when-let [matched-key (some #(and (= % v) %) (keys options-map))]
                     (get options-map matched-key)))))

(defn ensure-valid [{:keys [errors data]}]
  (if-not (empty? errors)
    (throw (ex-info "Validation error"
                    {:type :validation-error :reason errors}))
    data))

;; Apparently if I assign it ::undefined, I won't be able
;; to access the var and neither will it show in ns-interns.
(def undefined :undefined)

(defn validate
  ([state inputs k validators]
   (validate state inputs k validators undefined))

  ([state inputs k validators default-value]
   (let [initial-value (get inputs k)]
     (if (and (nil? initial-value) (not= default-value undefined))
       ;; Use default value if values wasn't provided.
       (update state :data merge {k default-value})

       ;; Validate provided value.
       (reduce
        (fn [state {:keys [type validator message]}]
          (let [current-value (get-in state [:data k])
                errors (get-in state [:errors k])]
            (if errors
              ;; Early termination: if previous validations failed,
              ;; don't try to validate the incorrect value any further.
              errors

              (try
                (if-let [validation-result (validator current-value)]
                  (update state :data merge {k validation-result})
                  (update state :errors merge {k {:type type :message message :value current-value}}))
                (catch Exception error
                  (println "\nError when running a custom validator")
                  (prn {:validator type :initial-value initial-value :current-value current-value})
                  (println)
                  (prn error)
                  (throw (ex-info "Error when running a custom validator"
                                  {:validator type :initial-value initial-value :current-value current-value})))))))
        (update state :data merge {k initial-value})
        validators)))))

(defn validate-or-default [m k validators default-value]
  (prn :DEPRECATED :validate-or-default k))

(defn validate-projections-keys [state inputs]
  (-> state
      (validate inputs :projections-duration
                [(generate-range-validator 1 5)] 1)
      (validate inputs :projections-start-date
                [timestamp-validator month-converter]
                (t/current-month))))

(defn validate-items [state inputs key-name validator-fn]
  (let [items (or (get inputs key-name) [])
        ;; Results should have the following format:
        ;; {:property-1 value-1 :property-2 value-2}

        ;; Errors should have the following format:
        ;; [{:error validators-results}], such as:
        ;; [{:error {:base-pay {:type :number, :message "must be 0 or larger", :value nil},
        ;;           :business-function {:type :string, :message "must be a string", :value nil}}}]
        results (mapv validator-fn items)
        grouped-results (group-by #(if (contains? % :error) :errors :ok) results)]
    (if (:errors grouped-results)
      (let [values (mapv :error (:errors grouped-results))
            message "The following values are not in the correct format"]
        (update state :errors merge {key-name {:message message :values values}}))
      (update state :data merge {key-name results}))))

(defn validate-rate-change [serialised-change]
  (if (re-matches #"\d+\|[^|]+\|[\d.]+" serialised-change)
    (let [[ts _ value] (str/split serialised-change #"\|")
          [ts value] [(Long/parseLong ts) (Double/parseDouble value)]]

      ;; Here we're bending the validate fn for our needs by giving it {:value v} instead of inputs.
      ;; It doesn't have sense to give it inputs, because properties that are lists are out of scope
      ;; for the validators to operate on. This is the way we get around it.
      (let [helper
            (fn [value validators]
              (let [result (validate {} {:value value} :value validators)]
                (get-in result [:data :value])))

            result
            {:effective-date (helper ts [timestamp-validator dt-converter])
             :new-value (helper value [double-validator])}]
        result))

    {:error serialised-change}))

(defn validate-rate-changes [state inputs key-name]
  (validate-items state inputs key-name validate-rate-change))
