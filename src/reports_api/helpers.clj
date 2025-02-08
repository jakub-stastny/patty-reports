(ns reports-api.helpers
  (:require [clojure.string :as str]))

(defn last-3-items [v]
  (let [n (count v)]
    (subvec v (max 0 (- n 3)) n)))

(defn penultimate-or-first [v]
  (if (< (count v) 2)
    (first v)
    (nth v (- (count v) 2))))

(defn last-3-penultimate [v]
  (let [n (count v)
        start (max 0 (- n 4)) ; Start 3 items before the last
        end (max 0 (- n 1))]  ; Exclude the last item
    (subvec v start end)))

(defn index-of [s item]
  (.indexOf (vec s) item))

(defn get-prev-item [s item]
  (let [sorted-seq (vec s) index (index-of s item)]
    (if (= index 0)
      (last sorted-seq) (get sorted-seq (dec index)))))

(defn select-months-until [months target-month]
  (reverse
   (take-while #(not= (:month (:month %)) target-month)
               (reverse months))))

(defn sum-vectors [v1 v2]
  (assert (or (and (vector? v1) (vector? v2))
              (and (seq? v1) (seq? v2)))
          (str "Arguments v1 and v2 must be vectors or sequences, got "
               (pr-str {:v1 v1 :v2 v2})))
  (assert (or (= (count v1) (count v2))
              (or (empty? v1) (empty? v2)))
          (str "Both v1 and v2 has to have the same number of items or one of them has to be empty, got "
               (pr-str {:v1 v1 :v2 v2})))
  (assert (and (every? number? v1) (every? number? v2))
          (str "Both v1 and v2 has to be all numeric, got "
               (pr-str {:v1 v1 :v2 v2})))

  (if (or (empty? v1) (empty? v2))
    (if (empty? v1) v2 v1)
    (mapv + (vec v1) (vec v2))))

(defn assoc-if-value-present [m k v]
  (if v (assoc m k v) m))

;; (defn transform-keys [m tfn]
;;   (let [tform #(keyword (str/join "-" (map name (tfn (name %)))))]
;;     (into {} (map (fn [[k v]] [(tform k) v]) m))))

(defn prn-ret
  ([thingy] (prn thingy) thingy)
  ([label thingy] (prn label thingy) thingy))

(defn get! [m k]
  (if (contains? m k)
    (get m k)
    (throw (ex-info "Key not found in map" {:key k :map m}))))

(defn validate-sums [vectors]
  (doseq [values (apply map vector vectors)] ; Iterate over corresponding elements
    (let [sum (reduce + values)]             ; Sum the elements at the current index
      (when (not= sum 1.0)
        (throw (ex-info "Sum at index does not equal 1.0"
                        {:values values :sum sum}))))))

(defn run-custom-fn [fun & args]
  (try
    (apply fun args)
    (catch Exception error
      (let [var-name (some-> fun class .getName)]
        (println (str "\nError in custom fn " var-name ":"))
        (println error)
        (throw error)))))

;; Round to 3 decimal places.
(defn round [double]
  (/ (Math/round (* double 1000.0)) 1000.0))

(defn calc-props [ns-sym props results & args]
  (reduce (fn [acc prop]
            (let [fn-name (symbol (str "calculate-" (name prop)))
                  resolved-fn (ns-resolve (find-ns ns-sym) fn-name)]
              (when-not resolved-fn
                (throw (ex-info "Missing calc fn" {:fn-name fn-name :ns ns-sym})))

              (assoc acc prop (apply resolved-fn (concat args [acc])))))
          results
          props))
