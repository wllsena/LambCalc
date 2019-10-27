#lang racket

(require "LambdaCalc.rkt")

;---

(% N0 (λ (f x) x))
(% N1 (λ (f x) (f x)))
(% N2 (λ (f x) (f (f x))))
(% N3 (λ (f x) (f (f (f x)))))
(% N4 (λ (f x) (f (f (f (f x))))))
(% N5 (λ (f x) (f (f (f (f (f x)))))))
(% N6 (λ (f x) (f (f (f (f (f (f x))))))))
(% N7 (λ (f x) (f (f (f (f (f (f (f x)))))))))
(% N8 (λ (f x) (f (f (f (f (f (f (f (f x))))))))))
(% N9 (λ (f x) (f (f (f (f (f (f (f (f (f x)))))))))))

;---

(% SUCC  (λ (n f x) f (n f x)))
(% PLUS  (λ (n m) m SUCC n))
(% MULT  (λ (n m) ((m (PLUS n)) N0))) 
(% TRUE  (λ (t f) t))
(% FALSE (λ (t f) f))
(% IS-0  (λ (x) (x (λ (y) FALSE)) TRUE))
(% PAIR  (λ (m n b) b m n))
(% FST   (λ (p) p TRUE))
(% SCD   (λ (p) p FALSE))
(% PRED  (λ (n f x) n (λ (g h) h (g f)) (λ (u) x) (λ (u) u)))
(% SUB   (λ (n m) m PRED n))
(% LEQ   (λ (m n) IS-0 (SUB m n)))
(% AND   (λ (n m) n m n))
(% OR    (λ (n m) n n m))
(% NOT   (λ (b t f) b f t))
(% EQUAL (λ (n m) (AND (LEQ n m) (LEQ m n))))

;---

(% NEXT (λ p (PAIR (PLUS (FST p) (SCD p)) (FST p))))
(% FIB1 (λ n SCD (n NEXT (PAIR N1 N0))))

;---

(% U (λ f f f))
(% B (λ (f g x) f (g x)))

;---

($!!)

;---

(% Y (λ f U (B f U)))

;---

(% FAC (Y (λ (f n) IS-0 n N1 (MULT n (f (PRED n))))))

(% FIB2 (U (λ (f n) LEQ n N2 N1 (PLUS (U f (PRED n))
                                     (U f (SUB n N2))))))

($!)

;---

(! (EQUAL (FIB1 N6) (FIB2 N6)))
(!.!! (FAC N3))
