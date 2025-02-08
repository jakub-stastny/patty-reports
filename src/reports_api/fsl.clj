(ns reports-api.fsl
  "FSL stands for formulae-specific language."
  (:require [clojure.string :as str]
            [reports-api.helpers :as h]))

(defmacro when-model [expected-revenue-model & body]
  `(if (= (:revenue-model ~'in) ~expected-revenue-model) (do ~@body) 0))

(defmacro if-subscription [if-yes if-not]
  `(if (= (:revenue-model ~'in) :subscription) ~if-yes ~if-not))

;; TODO: consider fetching the props vector automatically based on metadata.
(defmacro property [prop & body]
  `(h/defn-pass-name ~(symbol (str "calculate-" (name prop)))
     [~'fn-name ~'prev-months ~'month ~'in~'rs]
     ~@body))
