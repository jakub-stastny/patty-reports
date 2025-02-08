(ns jakub-stastny.extensions.case
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
