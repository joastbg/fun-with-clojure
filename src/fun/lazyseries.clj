;; Lazy sequences (can be rewritten using iterate and partial)

;; Lazy sequence of factorial
(defn facs [x s] (cons s (lazy-seq (facs (+ x 1) (* s x)))))
(take 10 (facs 2 1))

;; Lazy sequence of x to the power of n
(defn xton [x n] (cons (* x n) (lazy-seq (xton x (* x n)))))
(defn xton2 [x n] (cons (* x n) (lazy-seq (xton2 x (+ n 1)))))

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

(defn approx-sin-x [x n] 
  (reduce + (map #(%1 %2) (my-cycle [identity -]) (take-nth 2 (map #(with-precision 99 (/ %1 %2)) 
                                                                   (take n (xton x 1)) (facs 2M 1M))))))

;; atan


(defn approx-atan-x [x n] 
  (reduce + (map #(%1 %2) (my-cycle [identity -]) (take-nth 2 (map #(with-precision 99 (/ %1 %2)) 
                                                                   (take n (xton x 1))  (conj (xton2 1 2) 1))))))

(time (first (repeatedly 9999 #(identity (approx-atan-x 0.5M 99M)))))
"Elapsed time: 2.57714 msecs"
0.4636476090008061162142562314612112656006176910191297442548267721583007584272759900629875195675024791456671244096311843195720792926M

(time (first (repeatedly 9999 #(identity (Math/atan 0.5)))))
"Elapsed time: 0.242525 msecs"
0.4636476090008061

(map #(Math/atan (Math/pow 2 (- %1))) (range 0 28))
; (0.7853981633974483 0.4636476090008061 0.24497866312686414 0.12435499454676144 0.06241880999595735 0.031239833430268277 ...

