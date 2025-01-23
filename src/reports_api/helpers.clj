(ns reports-api.helpers
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn snake-to-kebab [key]
  "Converts a snake_case keyword to kebab-case."
  (-> key name (str/replace "_" "-") keyword))

(defn kebab-to-snake [key]
  "Converts a kebab-case keyword to snake_case."
  (-> key name (str/replace "-" "_")))

(defn transform-keys [trans-fn data]
  "Recursively transforms all map keys using the provided transformation function."
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (into {} (map (fn [[k v]] [(trans-fn k) v])) x)
       x))
   data))

(defn transform-keys-to-kebab-case [data]
  (transform-keys snake-to-kebab data))

(defn transform-keys-to-snake-case [data]
  (transform-keys kebab-to-snake data))

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

(defn assertions [fn-name value checks message]
  (assert (every? #(% value) checks)
          (str (name fn-name) ": " message ", got " (pr-str value))))

(defn assoc-if-value-present [m k v]
  (if v (assoc m k v) m))
