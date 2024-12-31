(ns reports-api.validators)

(defn make-validator [type message validator]
  {:type type :validator validator :message message})

(def number-validator
  (make-validator :number "must be 0 or larger"
                  #(and (number? %) (not (neg? %)) %)))

(def positive-number-validator
  (make-validator :positive-number "must be a positive number"
                  #(and (number? %) (pos? %) %)))

(def string-validator
  (make-validator :string "must be a string"
                  #(and (string? %) %)))

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
