(ns bot.core)

(def bearings [{:x 0 :y 1}
               {:x 1 :y 0}
               {:x 0 :y -1}
               {:x -1 :y 0}])

(defn bot [x y bearing-num]
  {:pos [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward  #(bot (+ x (:x (bearings bearing-num)))
                   (+ y (:y (bearings bearing-num)))
                   bearing-num)})

(defn power [base exp]
  (letfn [(f [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (f base exp 1)))

(def simple-metric {:meter 1,
                    :km 1000,
                    :cm 1/100,
                    :mm [1/10 :cm]})

(defn convert [context discriptor]
  (partition 2 discriptor))

(convert simple-metric [1 :meter 2 :km])

