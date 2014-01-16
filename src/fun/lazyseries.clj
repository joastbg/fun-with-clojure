;; Lazy sequences (can be rewritten using iterate and partial)

;; Lazy sequence of factorial
(defn facs [x s] (cons s (lazy-seq (facs (+ x 1) (* s x)))))
(take 10 (facs 2 1))

;; Lazy sequence of x to the power of n
(defn xton [x n] (cons (* x n) (lazy-seq (xton x (* x n)))))

(take 8 (xton 2 1))
(take 8 (xton 3 1))

;; Approximation of the number e
(defn approx-e [n] (reduce + (map #(with-precision 99 (/ %1 %2)) (take n (repeat 1)) (facs 1M 1M))))
(approx-e 99M)

(defn approx-e-x [x n] (+ 1M (reduce + (map #(with-precision 99 (/ %1 %2)) (take n (xton x 1)) (facs 2M 1M)))))
(Math/exp 2)
(approx-e-x 2M 99M)

(Math/exp 3)
(approx-e-x 3M 99M)

(Math/exp 5)
(approx-e-x 5M 99M)