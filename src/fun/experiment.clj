;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Application examples for algebraic data types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns clojure.contrib.types.examples
  (:use [clojure.contrib.types
	 :only (defadt match deftype)]))

;
; A simple tree structure defined as an algebraic data type
;
(defadt ::tree
  empty-tree
  (leaf value)
  (node left-tree right-tree))

(def a-tree (node (leaf :a) 
		  (node (leaf :b)
			(leaf :c))))

(defn depth
  [t]
  (match t
    empty-tree  0
    (leaf _)    1
    (node l r)  (inc (max (depth l) (depth r)))))

(depth empty-tree)
(depth a-tree)


(defadt ::expr
  evar
  (enum v)
  (esum e1 e2)
  (eprod e1 e2))

;; Differentiate
(defn deriv
	[e]
		(match e
			 evar     (enum 1)
			 (enum a) (enum 0)
			 (esum e1 e2) (esum (deriv e1) (deriv e2))
			 (eprod e1 e2) (esum (eprod e1, (deriv e2)) (eprod e2 (deriv e1)))))

;; Pretty printer
(defn str-of-expr [p e]
  (let [psum 10
        pprod 20]        
  (match e
         evar "x"
         (enum i) (str i)
         (esum e1 e2) (let [sum (str (str-of-expr psum e1) "+" (str-of-expr psum e2))]
                        (if (> p psum) (str "(" sum ")") sum))           
         (eprod e1 e2) (str (str-of-expr pprod e1) "*" (str-of-expr pprod e2)))))

;; Simplifier
(defn sim-sum [e]
  (match e
    (esum a b) a))

(defn sim-prod [e]
  (match e
    (eprod a b) (eprod a b)
    ))

; (eprod (enum 1) (enum 1))
(let [(eprod (enum 1) (enum 1)) (eprod (enum 1) (enum b))] b)

(defn sim-deriv [e]
  (match e
         evar     (enum 1)
         (enum a) (enum 0)
         ;(esum e1 e2) (sim-sum (esum (sim-deriv e1) (sim-deriv e2)))
         (esum e1 e2) (esum (sim-deriv e1) (sim-deriv e2))
			 (eprod e1 e2) (esum (sim-prod (eprod e1 (sim-deriv e2))) (sim-prod (eprod e2 (sim-deriv e1))))))

(deriv evar)
(deriv (enum (enum 1)))
(deriv (esum (enum 1) (enum 2)))
(deriv (eprod evar (enum 2)))

;; 1+2x

(def expr1 (esum (enum 1) (eprod (enum 2) evar)))
(str-of-expr 0 expr1)
(str-of-expr 0 (sim-deriv expr1))



;;;;;;;;;
