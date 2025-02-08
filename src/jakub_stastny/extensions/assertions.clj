(ns jakub-stastny.extensions.assertions
  (:require [clojure.string :as str]))

(defn assertions [fn-name value checks message]
  (assert (every? #(% value) checks)
          (str (name fn-name) ": " message ", got " (pr-str value))))

(defn assert-number [fn-name prop-name value]
  (assertions fn-name value [number?] (str "Variable " (name prop-name) " must be a number")))
