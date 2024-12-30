(ns reports-api.helpers
  (:require [clojure.string :as str]))

(defn snake-to-kebab [key]
  "Converts a snake_case keyword to kebab-case."
  (-> key
      name
      (str/replace "_" "-")
      keyword))

(defn transform-keys-to-kebab-case [data]
  "Recursively transforms all map keys from snake_case to kebab-case."
  (cond
    (map? data) (into {}
                      (map (fn [[k v]]
                             [(snake-to-kebab k) (transform-keys-to-kebab-case v)]))
                      data)
    (vector? data) (mapv transform-keys-to-kebab-case data)
    :else data))
