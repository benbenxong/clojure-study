(ns demo.futures
  (:use clojure.pprint))
;并发操作
;    协调  不协调
;同步 refs atoms
;异步 /    agents

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))

(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

;Atoms;;;;;;;;;;;;;

;先比较再设值（协调。阻塞）
;没有其它线程会看到atom的中间结果。
;swap! 会重试！
(def sarah (atom {:name "Sarah" :age 25 :wears-glasses? false}))
(swap! sarah update-in [:age] + 3)
(swap! sarah (comp #(update-in % [:age] inc)
                   #(assoc % :wears-glasses? true)))

;trying 5 会重试！
(def xs (atom #{1 2 3}))
(wait-futures 1
              (swap! xs (fn [v]
                            (Thread/sleep 250)
                            (println "trying 4")
                            (conj v 4)))
              (swap! xs (fn [v]
                          (Thread/sleep 500)
                          (println "trying 5")
                          (conj v 5))))

(def x (atom 2000))
;= #'user/x
(wait-futures 1
              (swap! x #(Thread/sleep %))
              (println @x)
              )

;不管当前值是什么，重新设值
(reset! x :y)

;Watches
(defn echo-watch
  [key identity old new]
  (println key old "=>" new))

(def sarah (atom {:name "Sarah" :age 25}))

(add-watch sarah :echo echo-watch)

(swap! sarah update-in [:age] inc)

(add-watch sarah :echo2 echo-watch)

(swap! sarah update-in [:age] inc)

(remove-watch sarah :echo2)

(swap! sarah update-in [:age] inc)

(reset! sarah @sarah)

;log watchs
(def history (atom ()))

(defn log->list
  [dest-atom key source old new]
  (when (not= old new)
    (swap! dest-atom conj new)))

(def sarah (atom {:name "Sarah" :age 25}))

(add-watch sarah :recode (partial log->list history))

(swap! sarah update-in [:age] inc)
(swap! sarah update-in [:age] inc)

(swap! sarah identity)

(swap! sarah assoc :wears-glasses? true)
(swap! sarah update-in [:age] inc)

(pprint @history)

;(defn log->db
;  [db-id identity old new]
;  (when (not= old new)
;    (let [db-connection (get-connection db-id)]
;      ...)))

;Validators
(def n (atom 1 :validator pos?))
(swap! n + 500)
(swap! n - 1000)

(def sarah (atom {:name "Sarah" :age 25}))
(set-validator! sarah :age)
(swap! sarah dissoc :age)

(set-validator! sarah #(or (:age %)
                           (throw (IllegalStateException. "People must have `:age`s!"))))

(swap! sarah dissoc :age)

;Refs;;;;;;;;;;;;;;;



