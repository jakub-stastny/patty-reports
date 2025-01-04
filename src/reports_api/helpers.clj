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
  (let [target (:month target-month)]
    (->> (reverse months)                           ; Process months in reverse
         (take-while #(not= (:month %) target))     ; Take all until target month
         (cons (first (filter #(= (:month %) target) months))) ; Add target month explicitly
         (remove nil?)                              ; Ensure no nil values in the result
         reverse                                    ; Reverse to the original order
         vec)))                                     ; Convert to a flat vector
