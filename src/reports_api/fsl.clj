(ns reports-api.fsl
  "FSL stands for formulae-specific language."
  (:require [clojure.string :as str]
            [jakub-stastny.extensions.define :refer [define]]
            [reports-api.helpers :as h]))

(defmacro when-model [expected-revenue-model & body]
  `(if (= (:revenue-model ~'in) ~expected-revenue-model) (do ~@body) 0))

(defmacro if-subscription [if-yes if-not]
  `(if (= (:revenue-model ~'in) :subscription) ~if-yes ~if-not))

(defmacro in! [key] `(h/get! ~'in ~key))
(defmacro rs! [key] `(h/get! ~'rs ~key))

;; TODO: wrap in try/catch.
(defmacro property [prop & body]
  `(define ~(symbol (str "calculate-" (name prop)))
     [~'fn-name ~'prev-months ~'month ~'in ~'rs]
     (let [result# (do ~@body)]
       (assert (not (nil? result#))
               ~(str "FSL property " prop " calculation returned nil"))
       result#)))
