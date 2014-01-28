(use '[clojure.core.match :only (match)])
(use '[clojure.contrib.types :only (defadt)])

(defn validates-credentials [username password]
  (let [uc (count username)
        pc (count password)]
    (match [username uc password pc]
       [(:or nil "") _ _ _]             {:error "No username given" :field "name"}
       [_ _ (:or nil "") _]             {:error "No password given" :field "pass"}
       [#"^([a-z0-9-_]+)$" _ _ _]       {:error "Username contains invalid characters" :field "name"}
       :else true)))

(defadt ::expr
  evar
  (enum v)
  (esum e1 e2)
  (eprod e1 e2))

;; Differentiate
(defn deriv
	[e]
		(match [e a b]			 
			 [enum _ _] (enum 0)
       [evar _ _] (enum 1)
			 [esum e1 e2] (esum (deriv e1) (deriv e2))
			 [eprod e1 e2] (esum (eprod e1, (deriv e2)) (eprod e2 (deriv e1)))))

;; Simplification
(defn simsum [expr]
  (match [(:type (:op1 expr)) (:value (:op1 expr)) (:type (:op2 expr)) (:value (:op2 expr))]
         [:num a :num b] {:type :num :value (+ a b)}
         [:num 0 :var _] {:type :var}
         [:var _ :num 0 ] {:type :var}
         :else expr))

(defn simprod [expr]
  (match [(:type (:op1 expr)) (:value (:op1 expr)) (:type (:op2 expr)) (:value (:op2 expr))]
         [:num a :num b] {:type :num :value (* a b)}
         [:num 0 _ _] {:type :num :value 0}
         [_ _ :num 0] {:type :num :value 0}
         [:num 1 _ _] (:op2 expr)
         [_ _ :num 1] (:op1 expr)
         :else expr))

(defn deriv2 [expr]
  (match [(:type expr) (:op1 expr) (:op2 expr)]
         [:var _ _] {:type :num :value 1}
         [:num _ _] {:type :num :value 0}
         [:sum a b] (simsum {:type :sum :op1 (deriv2 a) :op2 (deriv2 b)})
         [:prod a b] (simsum {:type :sum 
                              :op1 (simprod {:type :prod :op1 a :op2 (deriv2 b)}) 
                              :op2 (simprod {:type :prod :op1 b :op2 (deriv2 a)})})))

(deriv2 {:type :prod :op1 {:type :var} :op2 {:type :num :value 2}})

(defn str-of-expr [p e]
  (let [psum 10
        pprod 20]        
  (match e
         evar "x"
         (enum i) (str i)
         (esum e1 e2) (let [sum (str (str-of-expr psum e1) "+" (str-of-expr psum e2))]
                        (if (> p psum) (str "(" sum ")") sum))           
         (eprod e1 e2) (str (str-of-expr pprod e1) "*" (str-of-expr pprod e2)))))

(defn str2 [expr]
  (let [psum 10
        pprod 20]
  (match [(:type expr) (:op1 expr) (:op2 expr)]
         [:var _ _] "x"
         [:num _ _] (str (:value expr))
         [:sum a b] (str (str2 a) "+" (str2 b))
         [:prod a b] (str (str2 a) "*" (str2 b)) 
         )))

;; 2*x
(def expr1 {:type :prod :op1 {:type :prod :op1 {:type :var} :op2 {:type :num :value 2}} :op2 {:type :var}})
(str2 expr1)
(str2 (deriv2 expr1))

;; 1 + 2
(str2 {:type :sum :op1 {:type :num :value 1} :op2 {:type :num :value 2}})
(str (simsum {:type :sum :op1 {:type :num :value 1} :op2 {:type :num :value 2}}))

;; 0+x
(str2 {:type :sum :op1 {:type :num :value 0} :op2 {:type :var}})
(str2 (simsum {:type :sum :op1 {:type :num :value 0} :op2 {:type :var}}))
(str2 (simsum {:type :sum :op1 {:type :var} :op2 {:type :num :value 0}}))

;; x*0
(str2 {:type :prod, :op1 {:type :var}, :op2 {:type :num, :value 0}})
(str2 (simprod {:type :prod, :op1 {:type :var}, :op2 {:type :num, :value 0}}))

(str2 (simprod {:type :prod, :op1 {:type :var}, :op2 {:type :num, :value 1}}))
