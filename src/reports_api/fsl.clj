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

(defmacro property [prop & body]
  `(define ~(symbol (str "calculate-" (name prop)))
     [~'fn-name ~'prev-months ~'month ~'in ~'rs]
     (try
       (let [result# (do ~@body)]
         (assert (not (nil? result#))
                 ~(str "FSL property " prop " calculation returned nil"))
         result#)
       (catch clojure.lang.ExceptionInfo e#
         (println (str "Error in FSL property " ~prop ": " (.getMessage e#)))
         (println "Additional data:" (ex-data e#))
         (throw e#))
       (catch Throwable e#
         (println (str "Error in FSL property " ~prop ": " (.getMessage e#)))
         (throw e#)))))
