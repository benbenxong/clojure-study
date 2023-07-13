(ns demo.core
  (:import (com.sun.org.apache.bcel.internal.generic BIPUSH)))
(require 'clojure.java.io)
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
;; day2 1 ;;;;;;;;;;
(defn print-logger
  [wirter]
  #(binding [*out* wirter]
     (println %)))

(def *out*-logger (print-logger *out*))
(def writer (java.io.StringWriter.))

(defn file-logger
  [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
     ((print-logger f) %)))
(def log->file (file-logger "C:/coding/clojure/temp/messages.log"))

(log->file "hello")

(defn muti-logger
  [& logger-fns]
  #(doseq [f logger-fns]
     (f %)))

(def log (muti-logger
           (print-logger *out*)
           (file-logger "C:/coding/clojure/temp/messages.log")))

(defn timed-logger
  [logger]
  #(logger
     (format "[%1$tY-%1$tm-%1$te %1$tH:%1$tM:%1$tS] %2$s" (java.util.Date.) %)))

(def log-timed (timed-logger
                 (muti-logger
                   (print-logger *out*)
                   (file-logger "C:/coding/clojure/temp/messages.log"))))

(log-timed "hello hello")

(defn make-user
  [& [user-id name]]
  {:user-id (or user-id
               (str (java.util.UUID/randomUUID)))
   :name (or name
             "default-name")})

(make-user "abc" "alan")

(map-indexed vector "Clojure")

(filter (comp (partial > 25) :age)
        [{:age 21} {:age 20} {:age 34}])

(filter (partial > 25) [21,22,34])

(group-by #(rem % 3) (range 10))

(defn reduce-by
  [key-fn f init coll]
  (reduce (fn [summaries x]
            (let [k (key-fn x)]
              (assoc summaries k (f (summaries k init) x))))
          {} coll))


(def orders
  [{:product "Clock", :customer "Wile Coyote", :qty 6, :total 300}
   {:product "Dynamite", :customer "Wile Coyote", :qty 20, :total 5000}
   {:product "Shotgun", :customer "Elmer Fudd", :qty 2, :total 800}
   {:product "Shells", :customer "Elmer Fudd", :qty 4, :total 100}
   {:product "Hole", :customer "Wile Coyote", :qty 1, :total 1000}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Wile Coyote", :qty 6, :total 900}])

(reduce-by :customer #(+ %1 (:total %2)) 0 orders)
(reduce-by :product #(conj %1 (:customer %2)) #{} orders)

(reduce-by (juxt :customer :product)
           #(+ %1 (:total %2)) 0 orders)

(defn reduce-by-in
  [keys-fn f init coll]
  (reduce (fn [summaries x]
            (let [ks (keys-fn x)]
              (assoc-in summaries ks
                        (f (get-in summaries ks init) x))))
          {} coll))
(reduce-by-in (juxt :customer :product) #(+ %1 (:total %2)) 0 orders)

(reduce #(apply assoc-in %1 %2) {}
        (reduce-by (juxt :customer :product) #(+ %1 (:total %2)) 0 orders))
;; test
( (juxt :product :customer) (first orders))
(assoc-in {} ["abc" "ddd" "eee"] 300)
;; end test

(def x (transient []))
(count x)
(def y (conj! x 1))
(count y)
(persistent! y)

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))
(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

(def xs (atom #{1 2 3}))
(wait-futures 1 (swap! xs (fn [v]
                            (Thread/sleep 250)
                            (println "trying 4")
                            (conj v 4)))
              (swap! xs (fn [v]
                          (Thread/sleep 500)
                          (println "trying 5")
                          (conj v 5))))

(defn get-document
  [id]
  ; ... do some work to retrieve the identified document's metadata ...
  {:url "http://www.mozilla.org/about/manifesto.en.html"
   :title "The Mozilla Manifesto"
   :mime "text/html"
   :content (future (slurp "http://www.mozilla.org/about/manifesto.en.html"))})
(def d (get-document "some-id"))
(realized? (:content d))
@(:content d)

(def p (promise))
(def a (promise))
(def b (promise))
(def c (promise))

(future
  (deliver c (+ @a @b))
  (println "Delivery complete!"))

(deliver a 15)
(deliver b 15)

@c

(def a (promise))
(def b (promise))

(future (deliver a @b))
(future (deliver b @a))

(deliver a 42)

@b

(defn call-service
  [arg1 arg2 callback-fn]
  ; ...perform service call, eventually invoking callback-fn with results...
  (future (callback-fn (+ arg1 arg2) (- arg1 arg2))))

(defn sync-fn
  [async-fn]
  (fn [& args]
    (let [result (promise)]
      (apply async-fn (conj (vec args) #(deliver result %&)))
      @result)))

((sync-fn call-service) 8 7)

(defn phone-numbers
  [string]
  (re-seq #"(\d{3})[\.-]?(\d{3})[\.-]?(\d{4})" string))

(phone-numbers "231-333-3333")

(def files (repeat 100
                   (apply str
                          (concat (repeat 1000000 \space)
                                  "Sunil: 617.555.2937, Betty: 508.555.2218"))))
(time (dorun (map phone-numbers files)))

(time (dorun (pmap phone-numbers files)))

(def files (repeat 100000
                   (apply str
                          (concat (repeat 1000 \space)
                                  "Sunil: 617.555.2937, Betty: 508.555.2218"))))
(time (->> files
           (partition-all 250)
           (pmap (fn [chunk] (doall (map phone-numbers chunk))))
           (apply concat)
           dorun))

;;watch
(defn echo-watch
  [key identity old new]
  (println key identity old "==>" new))

(def sarah (atom {:name "Sarah" :age 25}))

(add-watch sarah :echo echo-watch)

(swap! sarah update-in [:age] inc)

(def n (atom 1 :validator pos?))

(swap! n + 400)

(swap! n - 1000)

(def m {:a 1, :b 2, :c 3})

;; ref ;;;;;;;;;;;;
(defn character
  [name & {:as opts}]
  (ref (merge {:name name :items #{} :health 500}
              opts)))

(def smaug (character "Smaug" :strength 400 :items (set (range 40))))
(def bilbo (character "Bilbo" :strength 100 :health 100 ))
(def gandalf (character "Gandalf" :mana 750 :health 75  ))

;; bilbo,gandalf 分smaug 的武器 :items
(defn loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (commute to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))

(wait-futures 1
              (while (loot smaug bilbo))
              (while (loot smaug gandalf)))
@smaug
@bilbo
@gandalf

(map (comp count :items deref) [bilbo gandalf])

(filter (:items @bilbo) (:items @gandalf))

;;;;;;;;;;;;;;;;;;;;;
(def x (ref 0))
(time (wait-futures 1
                    (dotimes [_ 1000]
                      (dosync (alter x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (alter x - (apply + (range 1000)))))
                    ))

(time (wait-futures 1
                    (dotimes [_ 1000]
                      (dosync (commute x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (commute x - (apply + (range 1000)))))
                    ))

;;;;;;;;;;;;;;;;;;;;;;