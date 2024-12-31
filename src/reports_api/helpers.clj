(ns reports-api.helpers
  (:require [clojure.string :as str]))

(defn snake-to-kebab [key]
  "Converts a snake_case keyword to kebab-case."
  (-> key name (str/replace "_" "-") keyword))

(defn kebab-to-snake [key]
  "Converts a kebab-case keyword to snake_case."
  (-> key name (str/replace "-" "_") keyword))

(defn transform-keys [trans-fn data]
  "Recursively transforms all map keys from snake_case to kebab-case."
  (cond
    (map? data) (into {}
                      (map (fn [[k v]]
                             [(trans-fn k) (transform-keys trans-fn v)]))
                      data)
    (vector? data) (mapv transform-keys trans-fn data)
    :else data))

(defn transform-keys-to-kebab-case [data]
  (transform-keys snake-to-kebab data))

(defn transform-keys-to-snake-case [data]
  (transform-keys snake-to-kebab data))
