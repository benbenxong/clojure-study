(ns demo.futures
  (:use clojure.pprint))
(require '[clojure.repl :refer [demunge]])
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

(defn character
  [name & {:as opts}]
   (ref (merge {:name name :items #{} :health 500}
               opts)))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))
;bilbo,gandalf 抢夺smaug :item所有武器！
;dosync 规定 STM 边界
;alter,commute,ref-set 对 ref 进行修改
;loot函数负责抢夺武器。
(defn loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (alter to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))

(wait-futures 1
              (while (loot smaug bilbo))
              (while (loot smaug gandalf)))

;验证正确性
(map (comp count :items deref) [bilbo gandalf] )
(filter (:items @bilbo) (:items @gandalf))

;COMMUTE 改善性能。commute提交时用最新值。不进行初始值与新值校验。
;commute 的函数符合交换率，就是可以没有前后之分。如+。先加后加谁都一样。
(def x (ref 0))
;232"
(time (wait-futures 5
                    (dotimes [_ 1000]
                      (dosync (alter x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (alter x + (apply + (range 1000)))))))
;59"
(time (wait-futures 5
                    (dotimes [_ 1000]
                      (dosync (commute x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (commute x + (apply + (range 1000)))))))
;ref加武器，没有顺序；取武器有顺序
(defn flawed-loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (commute to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(wait-futures 1
              (while (flawed-loot smaug bilbo))
              (while (flawed-loot smaug gandalf)))

;验证正确性
(map (comp count :items deref) [bilbo gandalf] )
(filter (:items @bilbo) (:items @gandalf))

;;other actions
(defn attack
  [aggressor target]
  (dosync
    (let [damage (* (rand 0.1) (:strength @aggressor))]
      (commute target update-in [:health] #(max 0 (- % damage))))))

(defn heal
  [healer target]
  (dosync
    (let [aid (* (rand 0.1) (:mana @healer))]
      (when (pos? aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (commute target update-in [:health] + aid)))))

;;demo
(def alive? (comp pos? :health))

(defn play
  [character action other]
  (while (and (alive? @character)
              (alive? @other)
              (action character other))
    (Thread/sleep (rand-int 50))))

;;action
(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo))

(map (comp :health deref) [smaug bilbo])

;; 多人战斗
(dosync
  (alter smaug assoc :health 500)
  (alter bilbo assoc :health 100)
  (alter gandalf assoc :mana 750))

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo)
              (play gandalf heal bilbo))

(map (comp #(select-keys % [:name :health :mana]) deref) [smaug bilbo gandalf])

;;修改ref. 与alter 相同 都会重试！
(dosync (ref-set bilbo {:name "Bilbo"}))
(dosync (alter bilbo (constantly {:name "Bilbo"})))

;;校验器
(defn- enforce-max-health
  [{:keys [name health]}]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already st max health!"))))))

(defn character
  [name & {:as opts}]
  (let [cdata (merge {:name name :items #{} :health 500}
                     opts)
        cdata (assoc cdata :max-health (:health cdata))
        validators (list* (enforce-max-health cdata)
                          (:validators cdata))]
    (ref (dissoc cdata :validators)
         :validator #(every? (fn [v] (v %)) validators))))

(def bilbo (character "Bilbo" :health 100 :strength 100))

(heal gandalf bilbo)

(get-validator bilbo)
(-> (get-validator bilbo)  ; get-validator returns the function object
    class                ; get the Class instance
    .getSimpleName       ; access the inner class name, no package
    demunge              ; demunge is the opposite of munge
    symbol)

;;改善heal
(defn heal
  [healer target]
  (dosync
    (let [aid (min (* (rand 0.1) (:mana @healer))
                   (- (:max-health @target) (:health @target)))]
      (when (pos? aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (commute target update-in [:health] + aid)))))

(dosync (alter bilbo assoc-in [:health] 95))
(heal gandalf bilbo)
(heal gandalf bilbo)

;;STM缺点：
;;dosync内不能有IO操作。（可以用io!将操作括起来）
(defn unsafe
  []
  (io! (println "writing to database...")))
(dosync (unsafe))

;;ref 不能是可变类型、
(def x (ref (java.util.ArrayList.)))

(wait-futures 2 (dosync (dotimes [v 5]
                          (Thread/sleep (rand-int 50))
                          (alter x #(doto % (.add v))))))
@x

;;barging
(def x (ref 0))
(dosync
  @(future (dosync (ref-set x 0)))
  (ref-set x 1))

(def a (ref 0))
(future (dotimes [_ 500] (dosync (Thread/sleep 200) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

;; var
;;meta data
;;exp1
(def ^{:private true} everthing 42)
everthing
;(ns other-ns)
(refer 'demo.futures)
@#'demo.futures/everthing

;;exp2:const
(def ^:const max-value 255)
(defn valid-value?
  [v]
  (<= v max-value))
(valid-value? 299)
(def max-value 500)
(valid-value? 299)

;;exp3:Dynamic Scope
(let [a 1
      b 2]
  (println (+ a b))
  (let [b 3
        + -]
    (println (+ a b))))

;Dynamic
(def ^:dynamic *max-value* 255)
(defn valid-value?
  [v]
  (<= v *max-value*))
(binding [*max-value* 500]
  (valid-value? 299))
*max-value*

(def ^:dynamic *var* :root)
(defn get-*var* [] *var*)
(binding [*var* :a]
  (binding [*var* :b]
    (binding [*var* :c]
      (get-*var*))))
(get-*var*)

(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (if (== 404 response-code)
      [response-code]
      [response-code (-> conn .getInputStream slurp)])))
(http-get "http://www.sian.com/bad-url")
(http-get "http://www.sina.com")

(def ^:dynamic *response-code* nil)
(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (when (thread-bound? #'*response-code*)
      (set! *response-code* response-code))
    (when (not= 404 response-code) (-> conn .getInputStream slurp))))
(http-get "http://www.sina.com")
(http-get "http://www.sina.com.cn/bad111_url")

(binding [*response-code* nil]
  (let [content (http-get "http://www.sina.com.cn/bad111_url")]
    (println "Response code was:" *response-code*)
    ; ... do something with `content` if it is not nil ...
    ))

;;惰性数组不支持 “绑定传播”
(binding [*max-value* 500]
  (map valid-value? [299]))
;;--> 解决办法
(map #(binding [*max-value* 500]
        (valid-value? %)) [299])

;;前置声明
;将核心函数放到最前面，具体小函数定义后面再定义。
(declare complex-helper-fn other-helper-fn)
(defn public-api-function
  [arg1 arg2]
  ...
  (other-helper-fn arg1 arg2 (complex-helper-fn arg1 arg2))
  (defn- complex-helper-fn
    [arg1 arg2]
    ...)
  (defn- other-helper-fn
    [arg1 arg2 arg3]
    ...)
