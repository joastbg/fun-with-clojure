(ns fun.currencies)

;; ----------------------------------------------------------------------
;; Experimental implementation of currencies
;; ----------------------------------------------------------------------

(def EURUSD (atom 1.3668))
(def USDJPY (atom 104.04))
(def GBPUSD (atom 1.6484))
(def USDCHF (atom 0.9026))
(def IDENDITY (atom 1.0))

(def rr {:USD {:EUR nil
               :JPY USDJPY
               :CHF USDCHF
               :GBP nil}
         :EUR {:USD EURUSD}
         :CHF {:USD nil}
         :JPY {:USD nil}
         :GBP {:USD GBPUSD}})

(defn convert [from to]
  (assoc from :oldamount (:amount from)
         :amount ((:converter (meta from)) from to)
         :currency to))

(defn cash [n c]
  (let [converter
        (fn [c d]
          ;; Fallback if error occurs, no conversion
          (let [x (d ((:currency c) rr))
                y ((:currency c) (d rr))
                z (if (nil? y) IDENDITY y)
                rate (if (nil? x) (-> z deref /) (-> x deref))]
            ;(do (println "DEBUG :: " rate " :: " n " :: " c)
          (* (:amount c) rate)))]
   (with-meta {:amount n :currency c} {:converter converter})))

;; ----------------------------------------------------------------------
;; Sample static usage
;; ----------------------------------------------------------------------

;; Convert 1000.00 USD to EUR
(def cash1 (cash 1000.00 :USD))
(def cash2 (cash 1000.00 :EUR))
(def cash3 (cash 1000.00 :JPY))

;; Round to arbitrary precision
(defn round [v d]
  (let [f (Math/pow 10 (- d 1))]
   (double (/ (Math/round (* v f)) f))))

(defn round [v d]
  (/ (Math/round (* v (Math/pow 10 d))) (double (Math/pow 10 d))))

;; Round quotes
(defn roundq [v]
  (round v 4))

;; Round amounts
(defn rounda [v]
  (round v 2))

;; ----------------------------------------------------------------------
;; Sample dynamic usage (background thread to simulate rates)
;; ----------------------------------------------------------------------

;; Generate random quite in interval
(defn randi [l u]
  (+ 0.10 l (* (- u l) (rand))))

;; Random periodic call to function fun
(defn periodicly [fun tf]
  "starts a thread that calls function every time ms"
  (let [thread (new Thread
     (fn [] (loop [] (fun) (Thread/sleep (* (rand) tf)) (recur))))]
        (.start thread) thread))

;; ----------------------------------------------------------------------
;; Test cases
;; ----------------------------------------------------------------------
(defn test-convert [c d]
  (let [fcash (cash 1000.0 c)
        acash (convert (convert fcash d) c)]
    (and (= (:currency acash (:currency fcash)))
         (let [round (fn [a]
                       (double (/ (Math/round (* a 100)) 100)))
               a1 (round (:amount fcash))
               a2 (round (:amount acash))]
         (= a1 a2)))))

(defn run-test-cases []
  (do
    (assert (test-convert :EUR :USD))
    (assert (test-convert :USD :GBP))
    (assert (test-convert :GBP :USD))
    (assert (test-convert :USD :JPY))
    (assert (test-convert :JPY :USD))
    (assert (test-convert :USD :CHF))
    (assert (test-convert :CHF :USD))))

(defn update-rates []
  (do
    (reset! EURUSD (randi 1.10 1.40))
    (println "New rate for EUR/USD: " @EURUSD)
    (reset! USDJPY (randi 100.0 120.0))
    (println "New rate for USD/JPY" @USDJPY)
    ))

(defn mapdoto [m k f]
  (assoc m k (-> (k m) f)))

(defn print-convert [cash to]
  (let [fmt (fn [kwd] (apply str (rest (str kwd))))]
    (do (let [m (convert cash1 to)]
      (println (str ":: "(fmt (:currency cash)) "->" (fmt to) " :: "
        (into (sorted-map) (assoc (mapdoto m :amount rounda)
          :rate (roundq (/ (:amount m) (:oldamount m)))))))))))

(defn get-and-convert []
  (do
    (print-convert cash1 :JPY)
    (print-convert cash2 :USD)
    (print-convert cash3 :USD)))

(defn -main [& args]
  (do
    (println "-- Testing a Currency implementation in Clojure\n")
    (println "   1. Running test cases")
    (run-test-cases)
    (println "   2. Starting simulation thread (separate threads for update and conversion)\n")
    (periodicly update-rates (randi 1000 1900))
    (periodicly get-and-convert 1000)))
