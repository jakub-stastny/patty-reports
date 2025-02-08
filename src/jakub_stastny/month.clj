(ns jakub-stastny.month
  (:refer-clojure :exclude [<= compare])
  (:require [jakub-stastny.extensions.define :refer [define]]
            [jakub-stastny.extensions.assertions :as a]))

(defn assert-month
  ([month] (assert-month :assert-month month))

  ([fn-name {:keys [year month] :as m}]
   (assert (int? year)
           (str (name fn-name) ": year must be a number, got " (pr-str m)))
   (assert (and (int? month) (clojure.core/<= 1 month 12))
           (str (name fn-name) ": month must be a number between 1 and 12, got " (pr-str m)))))

(defn assert-timestamp
  ([ts]
   (assert-timestamp :assert-timestamp ts))
  ([fn-name ts]
   (a/assertions fn-name ts [int?] "Timestamp must be a number")))

(define next-month
  ([_ month] (next-month month 1))

  ([fn-name month step]
   (assert-month fn-name month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot (dec total-months) 12)
         months (inc (mod (dec total-months) 12))]
     {:year years :month months})))

;; TODO: Will the pass macro work here?
(defn prev-month
  ([month] (prev-month month 1))

  ([month step]
   (assert-month :prev-month month)
   (let [total-months (+ (* (:year month) 12) (:month month) step)
         years (quot total-months 12)
         months (mod total-months 12)]
     {:year years :month months})))

(define format-month [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (format "%d-%02d" year month))

(define month-to-int [fn-name {:keys [year month] :as m}]
  (assert-month fn-name m)
  (+ (* year 12) month))

(define compare [fn-name m1 m2]
  (and (assert-month fn-name m1)
       (assert-month fn-name m2))
  (clojure.core/compare (month-to-int m1) (month-to-int m2)))

(define <= [fn-name & ms]
  (apply clojure.core/<= (map month-to-int ms)))
