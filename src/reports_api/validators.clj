(ns reports-api.validators)

(defn make-validator [type message validator]
  {:type type :validator validator :message message})

(def number-validator
  (make-validator :number "must be a positive number"
                  #(and (number? %) (not (neg? %)) %)))

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
