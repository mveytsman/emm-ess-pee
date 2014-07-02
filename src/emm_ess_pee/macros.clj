(ns emm-ess-pee.macros
  (:use emm-ess-pee.computer
        emm-ess-pee.binary-utils))
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

(defmacro def-unary-op
  "Defines a unary op with op-name and opcode. Inserts the created function into emm-ess-pee.instruction-set/unary-ops"
  [op-name opcode docstring [computer-binding byte?-binding register-binding value-binding] & body]
  `(let [op-fun# (with-meta
                   (fn [computer# byte?# source-mode# register#]
                     (let [~byte?-binding byte?#
                           ~register-binding register#
                           [~value-binding ~computer-binding] (get-value computer# source-mode# byte?# register#)]
                       ~@body))
                   {:doc ~docstring})]
     (alter-var-root (var emm-ess-pee.instruction-set/unary-ops)
                     #(assoc % ~opcode {:name (name (quote  ~op-name))
                                        :function op-fun#}))))




;; ;; (def-unary-op SWPB :opcode "000"
;; ;;   [computer register value]
;; ;;   (set-reg computer register (little-endian value)))


