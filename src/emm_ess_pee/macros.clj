(ns emm-ess-pee.macros)
;; Modified from the old clojure.contrib.cond
(defmacro cond-let
  "Takes a series of binding forms and then expressions. If the test
  in a binding form evaluates to true, the then expression is
  evaluated with the bindings. Use :else instead of a binding form for
  a default case"
  [& clauses]
  (when-let [[binding then & more] clauses]
    (if (= binding :else)
      then
      `(if-let ~binding
         ~then
         (cond-let ~@more)))))
