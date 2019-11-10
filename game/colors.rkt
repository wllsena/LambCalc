#lang racket

(require fancy-app racket/draw math/base)

;---

(define (square x) (expt x 2.0)) 

(define (rgb->lab R G B)
  (define sR  (/ R 255.0))
  (define sG  (/ G 255.0))
  (define sB  (/ B 255.0))

  (define st (λ (t) (* 100 (if (> t 0.04045)
                               (expt (/ (+ t 0.055) 1.055) 2.4)
                               (/ t 12.92)))))
  
  (define sR* (st sR))
  (define sG* (st sG))
  (define sB* (st sB))


  (define Xr (+ (* 0.4124564 sR*) (* 0.3575761 sG*) (* 0.1804375 sB*)))
  (define Yr (+ (* 0.2126729 sR*) (* 0.7151522 sG*) (* 0.0721750 sB*)))
  (define Zr (+ (* 0.0193339 sR*) (* 0.1191920 sG*) (* 0.9503041 sB*)))

  (define xr (/ Xr 95.0489))
  (define yr (/ Yr 100.0))
  (define zr (/ Zr 108.8840))

  (define s  #i6/29)

  (define f  (λ (t) (if (> t (expt s 3.0))
                        (expt t #i1/3)
                        (+ #i4/29 (/ t (* 3.0 (sqrt s)))))))

  (define fx (f xr))
  (define fy (f yr))
  (define fz (f zr))

  (define L (- (* 116 fy) 16))
  (define a (* 500 (- fx fy)))
  (define b (* 200 (- fy fz)))

  (values L a b))

;---

(define (delta-E L1 a1 b1 L2 a2 b2)
  (define C1       (sqrt (+ (square a1) (square b1))))
  (define C2       (sqrt (+ (square a2) (square b2))))
  (define delta-L  (- L1 L2))
  (define delta-C  (- C1 C2))
  (define delta-a  (- a1 a2))
  (define delta-b  (- b1 b2))
  (define d-sqrt-H (- (+ (square delta-a)
                         (square delta-b))
                      (square delta-C)))
  (define x (square delta-L))
  (define y (/ (square delta-C) (square (+ 1.0 (* 0.045 C1)))))
  (define z (/ d-sqrt-H (square (+ 1.0 (* 0.015 C1)))))

  (sqrt (+ x y z)))

;---
;----
;---

(struct lab-color (name L a b))

(define (get-lab-color name)
  (define color-rgb (send the-color-database find-color name))
  (define red       (send color-rgb red))
  (define green     (send color-rgb green))
  (define blue      (send color-rgb blue))
  (define-values (L a b) (rgb->lab red green blue))
  (lab-color name L a b))

(define (color-distance color1 color2)
  (match-define (lab-color _ L1 a1 b1) color1)
  (match-define (lab-color _ L2 a2 b2) color2)
  (delta-E L1 a1 b1 L2 a2 b2))

;---

(define (get-distances colors [result null])
  (cond
    [(null? colors) result]
    [else
     (define fst-color (car colors))
     (define distances
       (map (color-distance fst-color _) colors))
     (get-distances (rest colors) (cons distances result))]))
 

(define (sort-colors colors-name)
  (define colors-lab (map get-lab-color colors-name))
  (void))

(define (norm-string str)
  (define str* (string-replace str " " ""))
  (string-foldcase str*))

(define colors-name
  (let* ([names      (send the-color-database get-names)]
         [norm-names (map norm-string names)])
    (remove-duplicates norm-names string=?)))
    
(define colors-lab (map get-lab-color colors-name))
;(define distances  (get-distances colors-lab))

;---