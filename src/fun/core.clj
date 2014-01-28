(ns MonteCarlo.core
  (:import (org.apache.commons.math.random MersenneTwister))
  (:import (org.apache.commons.math.distribution NormalDistributionImpl))
  (:use (incanter core charts stats latex))
  (:require (org.apache.commons.math))
  (:require (cern.jet.random.Normal)))

(defn sayhello [] (print "hello"))

(- (* 2 (.nextDouble (MersenneTwister.))) 1)

(defn average [coll] 
  (/ (reduce + coll) (count coll)))

(defn mtrand [] (- (* 2 (.nextDouble (MersenneTwister.))) 1))
(def mt (MersenneTwister.))
(defn mtrand2 [] (.nextDouble mt))

;; Cern Jet-engine
(def agen (cern.jet.random.engine.MersenneTwister. 324234))
(def norm (cern.jet.random.Normal. 0 1 agen))
(defn jrand [] (.nextDouble norm))

;;Code courtesy of Chas Emerick
(let [slurp* (fn [f]
	       (with-open [#^java.io.BufferedReader r (java.io.BufferedReader. f)]
		 (let [sb (StringBuilder.)]
		   (loop [c (.read r)]
		     (if (neg? c)
		       (str sb)
		       (do (.append sb (char c))
			   (recur (.read r))))))))]
  (defn silent-read
    [s]
    (try
     (let [r (-> s java.io.StringReader. java.io.PushbackReader.)]
       [(read r) (slurp* r)])
     (catch Exception e))))
 
(defn interpolate-worker
  ([s atom?]
    (lazy-seq
      (if-let [[form rest] (silent-read (subs s (if atom? 2 1)))]
        (cons form (interpolate-worker (if atom? (subs rest 1) rest)))
        (cons (subs s 0 2) (interpolate-worker (subs s 2))))))
  ([#^String s]
    (let [start (max (.indexOf s "~{") (.indexOf s "~("))]
      (if (== start -1)
        [s]
        (lazy-seq (cons
		   (subs s 0 start)
		   (interpolate-worker (subs s start) (= \{ (.charAt s (inc start))))))))))
 
(defmacro interpolate-s
  [s]
  `(str ~@(interpolate-worker s)))

(jrand)
(rand)
(mtrand2)

;; Estimate the sample value of a price at maturity for an asset
(defn price_for_sample [s t r v rnd] (* s (Math/exp (+ (* (- r (* v v 0.5)) t) (* v rnd (Math/sqrt t))))))

;; Run Monte Carlo simulation in parallell
(average (take 1000000 (repeatedly #(price_for_sample 58.60 0.5 0.01 0.3 (mtrand)))))

(average (take 1000000 (repeatedly #(- 60 (price_for_sample 58.60 0.5 0.01 0.3 (jrand))))))

(defn mcsim [n] (average (take n (repeatedly #(- (price_for_sample 58.60 0.5 0.01 0.3 (mtrand2)) 60)))))

(take 10 (repeatedly #(- (price_for_sample 58.60 0.5 0.01 0.3 (rand)) 60)))

(take 10 (repeatedly #(price_for_sample 58.60 0.5 0.01 0.3 (jrand))))

(price_for_sample 58.60 0.5 0.01 0.3 1.0)

;; s: stock price
;; x: strike price of option
(defn payoff_call [s x] (Math/max (- s x) 0.0))

;; New try to get it right, random value (0,1)
(average (take 1000 (repeatedly #(payoff_call (price_for_sample 58.60 0.5 0.01 0.3 (mtrand2)) 60))))

(take 5 (repeatedly #(payoff_call (price_for_sample 58.60 0.5 0.01 0.3 (mtrand2)) 60)))

(Math/max 1 0)

(mcsim 1000)

(def nd (org.apache.commons.math.distribution.NormalDistributionImpl. 0 1))
(.sample nd)
(.sample (org.apache.commons.math.distribution.NormalDistributionImpl. 0 1))

;; Distribute job 
(defn pmcsim [p n] (average (pmap mcsim (take p (repeat n)))))

;; View Latex rendered equation
(view (latex "f(x)=\\frac {1} {\\sqrt {2\\pi \\sigma ^2}} e^{\\frac {-(x - \\mu)^2}{2 \\sigma ^2}}"))


;; Black Scholes 4.46
(time (mcsim 1000000))
(time (pmcsim 8 100000))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(require '[clojure.tools.nrepl :as repl])
(with-open [conn (repl/connect :port 1435)]
     (-> (repl/client conn 1000)    ; message receive timeout required
       (repl/message {:op "eval" :code "(+ 2 3)"})
       repl/response-values))

(with-open [conn (repl/connect :port 1435)]
     (-> (repl/client conn 1000)
       (repl/message {:op :eval :code "(time (reduce + (range 1e6)))"})
       doall      ;; `message` and `client-session` all return lazy seqs
       print))
       

