(ns ramp-up.core
  (:require [incanter.core :refer :all]
            [incanter.io :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all])
  (:use [clojure.test])
  )

(defn pulse-step-fn
  "Stepper pulse generation function that always outputs a 60kHz signal"
  [step]
  60000)

(defn pulse-linear-fn
  "Stepper pulse generation function that uses a linear ramp-up"
  [step]
  (let [slope 200
        initial_offset 800]
    (min
     (+ (* step slope) initial_offset) ; y = mx+b
     80000 ; max 80 kHz
     )))

(defn pulse-logistic-fn
  "Stepper pulse generaiton function that uses a logistic-function shape for ramp-up"
  [step]
  (let [e 2.71828
        L 120000 ; maximum value for the curve
        x_0 (/ 800 2)                  ; x-value of the sigmoid's midpoint
        k 0.01 ; logistic growth rate / steepness of curve
        ]
    ;; f(x) = L / (1 + e^(-k(x-x_0
    ;; https://en.wikipedia.org/wiki/Logistic_function
    (/ L
       (+ 1 (pow e (* (- k) (- step x_0)))))))

(with-test
  (defn hz-to-ns
    "Converts from hertz to nanoseconds. For usage with stepper signal generation."
    [hertz]
    (* 1000000000 (/ 1 hertz)))
  (is (= (double (hz-to-ns 1)) (pow 10 9)))
  (is (< (- (hz-to-ns 60000) 16667) ; technically 50000/3, so check that it's close enough to 16667 ns
         1)))

(with-test
  (defn precompute-pulse
    "Takes a pulse-generation function and turns it into a vector with the given number of steps"
    [pulse-fn steps]
    (let [halfway (vec (map pulse-fn (range 1 (inc (/ steps 2))))) ; split up the steps in half so that we can have acceleration and then deceleration. The purpose of this is to provide smooth motion and avoid missed steps.
          second-half (-> (if (odd? steps)
                            (pop halfway) ; if it's an odd number drop off the first step (the one that will end up in the middle) before reversing. This is so that we end up with the correct number of steps.
                            halfway)
                          (reverse))
          ]
      (vec (concat halfway second-half))
      ))
  (let [testing-fn (fn [step] (- step)) ; simple testing function the negative is so that we can easily observer that the testing-fn actually got called.
        ]
    (is (= (precompute-pulse testing-fn 4) [-1 -2 -2 -1]))
    (is (= (precompute-pulse testing-fn 5) [-1 -2 -3 -2 -1]))))

;; generate the plots
(def step-plot (function-plot pulse-step-fn 0 800 :title "Stepwise" :x-label "Step #" :y-label "Hertz"))
;;(view step-plot)
(def linear-plot (function-plot pulse-linear-fn 0 800 :title "Linear" :x-label "Step #" :y-label "Hertz"))
;;(view linear-plot)
(def logistic-plot (function-plot pulse-logistic-fn 0 800 :title "Logistic" :x-label "Step #" :y-label "Hertz"))
;;(view logistic-plot)

(defn make-plot [hz-vals]
  (let [hzs-for-dataset (vec (map-indexed
                              (fn [idx itm] [idx itm])
                              hz-vals))]
    (with-data
      (dataset ["Step" "Hz"] hzs-for-dataset)
      (line-chart "Step" "Hz")
      )))


(def four-turn-linear-plot (make-plot (precompute-pulse pulse-linear-fn 3200)))
;;(view four-turn-linear-plot)
(def four-turn-logistic-plot (make-plot (precompute-pulse pulse-logistic-fn 3200)))
;;(view four-turn-logistic-plot)

;; to use the pre-computed pulses to drive a signal, do something like the following:
;; (defn move-by-pulses [num-pulses]
;;   (let [id (normalize-pin-tag (:id args))
;;         precomputed-pulses (precompute-pulse pulse-step-fn num-pulses)] 
;;     (do
;;       (doseq [pulse-val precomputed-pulses]
;;         (do
;;           (set-pin pul true)
;;           (java.util.concurrent.locks.LockSupport/parkNanos (hz-to-ns pulse-val))
;;           (set-pin pul false)
;;           (java.util.concurrent.locks.LockSupport/parkNanos (hz-to-ns pulse-val)))))))
;; hz-to-ns could be used in the calculation of precomputed-pulses to make it even faster
