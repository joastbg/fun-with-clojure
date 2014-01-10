;; Bond valuation experiement

;; Continuous discount factor
(def df (fn [r T] (Math/exp (- (* r T)))))
 
;; This works for annuity example
(reduce + (map #(* % 1000) (map #(df (Math/log 1.05) %) [1 2 3 4 5])))
 
;; Fixed coupon bond with interest rates from list
(def irs [3.0 3.25 3.5 3.55 3.3])
(reduce + (map #(* %1 %2) [100 100 100 100 1100] (map #(df (Math/log (+ 1 (/ %1 100))) %2) irs (range 1 6))))
 
;; DFs as lazy list
(defn dfs [r t] (cons (df r t) (lazy-seq (dfs r (+ t 1)))))
(reduce + (map #(* % 1000) (take 5 (dfs (Math/log 1.05) 1))))
 
;; using hash-map
{:maturity 5, :face 1000, :coupons [3.0 3.25 3.5 3.55 3.3]}
(defn fcb [m c f] (hash-map :maturity m :couponr c :face f))
(fcb 10 0.05 1000)
 
;; Fixed coupon bond, interest rates from list
(def sam1 {:maturity 5, :face 1000, :rates irs, :coupon 10})
;; Annuity
(def sam2 {:maturity 5, :face 1000, :rates (take 5 (repeat 5)) :coupon 100})
 
;; Generate coupons maturity, coupon and face
(defn gcs [m c f] (concat (take (- m 1) (cycle [c])) [(+ f c)]))
(gcs 5 100 1000)
 
(reduce + (map #(* %1 %2) (gcs 5 100 1000) (map #(df (Math/log (+ 1 (/ %1 100))) %2) irs (range 1 6))))
 
;; Enhanced interface
;(defn pv [cs dfs] (reduce + (map #(* %1 %2) cs dfs)))
(defn pv [cs dfs] (reduce + (map * cs dfs)))
(def dfs (map #(df (Math/log (+ 1 (/ %1 100))) %2) irs (range 1 6)))
(pv (gcs 5 100 1000) dfs)
 
;; Generate discount factors from list of interest rates
(map #(df (Math/log (+ 1 (/ %1 100))) %2) irs (iterate inc 1))
;; Refactor to helper function for df
(defn dfi [rs] (map #(df (Math/log (+ 1 (/ %1 100))) %2) rs (iterate inc 1)))
(dfi irs)
 
;; Present Value combining two lists cfs and dfs
(pv (gcs 5 100 1000) (dfi irs))
 
;; Logic to calulate present value from input
(pv (gcs (:maturity sam1) (* (/ (:coupon sam1) 100) (:face sam1)) (:face sam1)) (dfi (:rates sam1)))
 
;; Calculates PV from Bond hash
(defn bpv [bond] (pv (gcs (:maturity bond) (* (/ (:coupon bond) 100) (:face bond)) (:face bond)) (dfi (:rates bond)))) ;; for fixed coupon bonds
(defn bpva [bond] (pv (gcs (:maturity bond) (* (/ (:coupon bond) 100) (:face bond)) 0) (dfi (:rates bond)))) ;; for annuities
 
(bpv sam1)
(bpv sam2) ;; not working, since we add the face as repayment...
(bpva sam1) ;; works, custom function for annuities