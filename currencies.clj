;; ----------------------------------------------------------------------
;; Experimental implementation of currencies
;; ----------------------------------------------------------------------

(def EURUSD (atom 1.3668))
(def USDJPY (atom 104.04))
(def GBPUSD (atom 1.6484))
(def USDCHF (atom 0.9026))

(deref EURUSD)

(def t1 *)
(def t1 /)

;; TODO: Handle updates using lambda
(def rr {:USD {:EUR [EURUSD /]
               :JPY [USDJPY *]
               :CHF [USDCHF *]
               :GBP [GBPUSD /]}
         :EUR {:USD [EURUSD *]}
         :CHF {:USD [USDCHF /]}
         :JPY {:USD [USDJPY /]}
         :GBP {:USD [GBPUSD *]}})


(first (:EUR (:USD rr)))
(:EUR (:USD rr))
(last '(EURUSD /))
;(def test (with-meta {:EUR EURUSD} {:modifier t1})) ((:modifier (meta
;test)) (:EUR test))
(-> EURUSD deref /)

(-> (first (:EUR (:USD rr))) deref ((last (:EUR (:USD rr)))))
(-> (first (:USD (:EUR rr))) deref ((last (:USD (:EUR rr)))))

(defn convert [c d]
  (assoc c :amount ((:converter (meta c)) c d) :currency d))

(defn cash [n c]
  (let [converter
        (fn [c d]
          (* (:amount c)
             (-> (first (:USD (:EUR rr))) deref ((last (:USD (:EUR rr)))))))]
   (with-meta {:amount n :currency c} {:converter converter})))

;; ----------------------------------------------------------------------
;; Sample static usage
;; ----------------------------------------------------------------------

;; Convert 1000.00 USD to EUR
(def cash1 (cash 1000.00 :USD))
(convert cash1 :EUR)

;; Convert 1000.00 USD to EUR then back again
(convert (convert (cash 1000.00 :USD) :EUR) :USD)

;; ----------------------------------------------------------------------
;; Sample dynamic usage (background thread to simulate rates)
;; ----------------------------------------------------------------------

;; Alter the rate for EURUSD and convert again
(swap! EURUSD inc)
(convert cash1 :EUR)

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

(test-convert :EUR :USD)
(test-convert :USD :EUR)

(test-convert :USD :GBP)
(test-convert :GBP :USD)

(test-convert :USD :JPY)
(test-convert :JPY :USD)

(test-convert :USD :CHF)
(test-convert :CHF :USD)
