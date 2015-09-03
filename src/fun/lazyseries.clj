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

;; e^x = 1 + x + x^2 / 2! 0 x^3 / 3! + ...
(defn approx-e-x [x n] (+ 1M (reduce + (map #(with-precision 99 (/ %1 %2)) (take n (xton x 1)) (facs 2M 1M)))))
(Math/exp 2)
(approx-e-x 2M 99M)

(Math/exp 3)
(approx-e-x 3M 99M)

(Math/exp 5)
(approx-e-x 5M 99M)


;; from clojure.contrib

(defmacro rec-seq 
 "Similar to lazy-seq but binds the resulting seq to the supplied 
  binding-name, allowing for recursive expressions."
 [binding-name & body]
  `(let [s# (atom nil)]
     (reset! s# (lazy-seq (let [~binding-name @s#] ~@body)))))
   
(defn my-cycle [coll] (rec-seq c (concat coll c)))
;(map #(%1 %2) (my-cycle [...]) (facs 2 1))

(defn map-every-nth [f coll n]
  (map f (take-nth n coll)))

;; --- a few trig functions (need refactoring)

;; cosh

(defn approx-cosh-x [x n] 
  (+ 1M (reduce + (take-nth 2 (drop 1 (map #(with-precision 99 (/ %1 %2)) 
                                           (take n (xton x 1)) (facs 2M 1M)))))))

;; sinh

(defn approx-sinh-x [x n] 
  (reduce + (take-nth 2 (map #(with-precision 99 (/ %1 %2)) 
                             (take n (xton x 1)) (facs 2M 1M)))))

;; cos

(defn approx-cos-x [x n] 
  (+ 1M (reduce + (map #(%1 %2) (my-cycle [- identity]) 
                       (take-nth 2 (drop 1 (map #(with-precision 99 (/ %1 %2)) (take n (xton x 1)) (facs 2M 1M))))))))

;; sin

(defn approx-cos-x [x n] 
  (reduce + (map #(%1 %2) (my-cycle [- identity]) (take-nth 2 (map #(with-precision 99 (/ %1 %2)) 
                                                                   (take n (xton x 1)) (facs 2M 1M))))))
