(ns demo.macroTest)
(require '(clojure [string :as str]
                   [walk :as walk]))

(defmacro reverse-it
  [form]
  (walk/postwalk #(if (symbol? %)
                    (symbol (str/reverse (name %)))
                    %)
                 form))
(reverse-it (nltnirp "foo"))
;;test
(macroexpand-1 '(reverse-it (nltnirp "foo")))