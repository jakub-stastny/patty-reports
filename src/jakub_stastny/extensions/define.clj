(ns jakub-stastny.extensions.define
  "Defines macro define that passes function name as the first argument.
  From there on, it behaves precisely like the built-in defn.")

(defmacro define [fn-name args & body]
  (if (vector? args)

    ;; Single arity case
    (let [internal-args (vec (rest args))]
      `(def ~(with-meta fn-name (meta fn-name))
         (let [name# '~fn-name]
           (fn ~internal-args
             (let [~(first args) name#]
               ~@body)))))

    ;; Multi-arity case
    (let [process-arity (fn [[args & body]]
                          (let [internal-args (vec (rest args))]
                            `(~internal-args
                              (let [~(first args) '~fn-name]
                                ~@body))))]
      `(def ~(with-meta fn-name (meta fn-name))
         (fn ~@(map process-arity (cons args body)))))))
