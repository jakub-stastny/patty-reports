(ns reports-api.validators)

(def number-validator
  {:type :number :validator #(and (number? %) (not (neg? %))) :message "must be a positive number"})

(defn- throw-validation-error [m k v]
  (throw (ex-info "Validation error" {:type :validation-error :reason m :key k :value v})))

(defn validate [m k validators]
  (let [v (get m k)]
    (doseq [{:keys [type validator message]} validators]
      (when-not (validator v)
        (println (str "Validator " type " failed for " k " = " (pr-str v)))
        (throw-validation-error message k v)))))
