(ns concurrency.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def yak-butter-international
  {:store "Yak Butter International"
   :price 90
   :smoothness 90})

(def butter-than-nothing
  {:store "Butter Than Nothing"
   :price 150
   :smoothness 83})

(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

;(time (some (comp satisfactory? mock-api-call) [yak-butter-international butter-than-nothing baby-got-yak]))



;; (time
;;  (let [butter-promise (promise)]
;;    (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
;;      (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
;;                (deliver butter-promise satisfactory-butter))))
;;    (prn "And the winner is: " @butter-promise)))

(let [p (promise)]
  (future
    (prn "Waiting on the promise...")
    (println "promise is " @p))
  (Thread/sleep 1000)
  (deliver p 12))
