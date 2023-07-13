(ns demo.futures)
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

;Atoms
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

