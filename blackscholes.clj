;; Black Scholes

(defn cnd [x]
  "Cumulative distribution function"
  (let [a1 0.31938153
        a2 -0.356563782
        a3 1.781477937
        a4 -1.821255978
        a5 1.330274429
        pi 3.141592654
        l (Math/abs x)
        k (/ 1 (+ 1 (* 0.2316419 l)))
        w1 (/ 1 (Math/sqrt (* 2 pi)))
        w2 (Math/exp (- (* l l 0.5)))
        w3 (* a3 (Math/pow k 3.0))
        w4 (* a4 (Math/pow k 4.0))
        w5 (* a5 (Math/pow k 5.0))
        w (- 1 (* w1 w2 (* (+ w5 w4 w3 (* k k a2) (* k a1)))))]
    (if (< x 0) (- 1 w) w)))

(defn bs [f s x t r v]
  "Black-Scholes"
  (let [d1 (/ (+ (java.lang.Math/log (/ s x)) (* (+ r (* v v 0.5)) t)) (* v (Math/sqrt t)))
        d2 (- d1 (* v (Math/sqrt t)))]    
    (if (= f 'p') 
      (- (* x (Math/exp (* (- r) t)) (cnd (- d2))) (* s (cnd (- d1)))) ; Put
      (- (* s (cnd d1)) (* x (Math/exp (* (- r) t)) (cnd d2)))))) ; Call

;; Usage
(bs 'c' 58.60 60.0 0.5 0.01 0.3)
(bs 'p' 58.60 60.0 0.5 0.01 0.3)