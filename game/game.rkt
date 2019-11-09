#lang racket/gui

(require "../main.rkt" fancy-app racket/draw rsound)

;;; Abbreviations
; rlt    -> relative
; childs -> children
; lamb   -> lambda expression
; symb   -> symbol
; wd     -> width
; hg     -> heigt
; norm   -> normalizer
; bg     -> background
; specs  -> specifications
; dif    -> difference

;---

(define cv-width  1000)
(define cv-heigth 1000)

;---

(define sounds
  (list
   (rs-read "sounds/error_sound.wav")))  ;0 error-sound

(define colors
  (list
   (make-object color% 225 169 84)       ;0
   (make-object color% 178 139 100)      ;1
   (make-object color% 98  116 93)       ;2
   (make-object color% 36  139 64)       ;3
   (make-object color% 1   75  13)       ;4 
   (make-object color% 75  181 67 3/4)   ;5 red-error
   (make-object color% 192 32  32 3/4))) ;6 green-sucess

;---

(define (draw-rounded-rectangle dc x y width heigth)
  (let* ([path     (new dc-path%)]
         [curve-to (λ (x1 y1 x2 y2 x3 y3)
                     (send path curve-to
                           (* width x1) (* heigth y1)
                           (* width x2) (* heigth y2)
                           (* width x3) (* heigth y3)))])
    (send path move-to 0 (* 0.1 heigth))
    (curve-to 0     0.05  0.05  0     0.1   0)
    (curve-to 0.2   -0.02 0.8   -0.02 0.9   0)
    (curve-to 0.95  0     1     0.05  1     0.1)
    (curve-to 1.02  0.2   1.02  0.8   1     0.9)
    (curve-to 1     0.95  0.95  1     0.9   1)
    (curve-to 0.8   1.02  0.2   1.02  0.1   1)
    (curve-to 0.05  1     0     0.95  0     0.9)
    (curve-to -0.02 0.8   -0.02 0.2   0     0.1)
    (send path translate x y)
    path))

;---
;----- GUI
;---

(define frame
  (new frame%
       [label "Game"]))

(define message
  (new message% [parent frame]
       [label "Hello!"]
       [stretchable-width #t]))

(define panel
  (new horizontal-panel% [parent frame]))
 
(define text-field
  (new text-field% [parent panel]
       [label "Lambda expression: "]
       [init-value "(λ x . x) (λ x . x)"]))

(define button-run
  (new button% [parent panel]
       [label "Run!"]
       [callback (λ _
                   (send message set-label "Hello")
                   (send canvas draw))]))
#|
(define button-rm-elders
  (new button% [parent panel]
       [label "Retire Elders"]
       [callback (λ _ (remove-elders root)
                   (send canvas redraw))]))
|#
;---
;----- CANVAS
;---

(define canvas
  (new
   (class canvas%
     (inherit get-dc)

     (define/public (draw)
       (define expr (get-expr))
       (when expr
         (define node (get-nodes expr))
         (redraw node)))

     (define/public (redraw node)
       (define node*  (get-sizes node))
       (match-define  (specs _ _ root-wd root-hg _) (obtain-spcs node*))
       (define norm   (min (/ cv-width  root-wd) (/ cv-heigth root-hg)))
       (define x      (/ (- cv-width (* norm root-wd)) 2))
       (define y      (/ (- cv-heigth (* norm root-hg)) 2))
       (define node** (get-specs node* x y norm))
       
       (define dc (get-dc))
       (send dc set-background "DarkGray")
       (send dc clear)
       (send dc set-smoothing 'aligned)
       (draw-bgs dc node**))
     #|
     (define bg-selected #f)

     (define/override (on-event event)
       (define dc (get-dc))
       (cond
         [(send event button-up?)
          (when bg-selected 
            (define bg-posit
              (match-background (send event get-x) (send event get-y)))
            (try-to-reduce dc bg-selected bg-posit)
            (redraw))
          (set! bg-selected #f)]
         [(send event button-down?)
          (define bg-posit
            (match-background (send event get-x) (send event get-y)))
          (if (hash-ref arguments bg-posit)
              (set! bg-selected bg-posit)
              (display-answer dc bg-posit 6))]))
      |#
     (super-new
      [parent     frame]
      [min-width  (+ 20 cv-width)]
      [min-height (+ 20 cv-heigth)]
      [paint-callback
       (λ (_ dc)
         (send dc set-origin 10 10)
         (draw))]))))

;---

(define (invalid-expression exn)
  (send message set-label "INVALIDE EXPRESSION!")
  #f)

(define (get-expr)
  (define text (send text-field get-value))
  (with-handlers ([exn:fail? invalid-expression])
    (α (rw-expr (string->expr text)))))

;---

(struct specs          (x y width heigt color)    #:transparent)
(struct symb           (expr               specs) #:transparent)
(struct lamb           (expr arg    child  specs) #:transparent)
(struct bracket        (expr child1 child2 specs) #:transparent)

;---

(define (one-color-available parent-color)          
  (define available-colors
    (remove parent-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

;---

(define (get-nodes expr [parent-color 5])
  (define color (one-color-available parent-color))
  (match expr
    [(? symbol?)
     (symb expr (specs 0 0 0 0 color))]

    [`(λ ,arg ,body)
     (define child (get-nodes body color))
     (lamb expr arg child (specs 0 0 0 0 color))]

    [`(,expr1 ,expr2)
     (define child1 (get-nodes expr1 color))
     (define child2 (get-nodes expr2 color))
     (bracket expr child1 child2 (specs 0 0 0 0 color))]))

;---

(define obtain-spcs
  (match-lambda
    [(or (symb _ spcs)
         (lamb _ _ _ spcs)
         (bracket _ _ _ spcs))
     spcs]))

(define obtain-childs
  (match-lambda
    [(symb _ _)
     #f]
    [(lamb _ _ child _)
     (list child)]
    [(bracket _ child1 child2 _)
     (list child1 child2)]))

;---

(define get-sizes
  (match-lambda
    [(symb expr (specs _ _ _ _ color))
     (symb expr (specs 0 0 100 100 color))]
    
    [(lamb expr arg child (specs _ _ _ _ color))
     (define child* (get-sizes child))
     (match-define (specs _ _ wd hg _) (obtain-spcs child*))
     (lamb expr arg child*
           (specs 0 0 (* 22/20 wd) (* 23/20 (+ 100 hg))
                  color))]
    
    [(bracket expr child1 child2 (specs _ _ _ _ color))
     (define child1* (get-sizes child1))
     (define child2* (get-sizes child2))
     (match-define (specs _ _ wd1 hg1 _) (obtain-spcs child1*))
     (match-define (specs _ _ wd2 hg2 _) (obtain-spcs child2*))
     (bracket expr child1* child2*
              (specs 0 0 (* 23/20 (+ wd1 wd2)) (* 22/20 (max hg1 hg2))
                     color))]))

;---

(define (get-specs node x y norm)
  (match node
    [(symb expr (specs _ _ wd hg color))
     (symb expr (specs x y (* norm wd) (* norm hg) color))]

    [(lamb expr arg child (specs _ _ wd hg color))
     (let* ([n-wd   (* norm wd)]
            [n-hg   (* norm hg)]
            [n-x    (+ x (* 1/22 n-wd))]
            [n-y    (+ y (* 100 norm) (* 2/23 n-hg))]
            [child* (get-specs child n-x n-y norm)])
       (lamb expr arg child*
             (specs x y n-wd n-hg color)))]

    [(bracket expr child1 child2 (specs _ _ wd hg color))
     (match-define (specs _ _ _ (app (* norm _) hg1) _)
       (obtain-spcs child1))
     (match-define (specs _ _ _ (app (* norm _) hg2) _)
       (obtain-spcs child2))
     (let* ([n-wd    (* norm wd)]
            [n-hg    (* norm hg)]
            [n-x     (+ x (* 1/23 n-wd))]
            [n-y     (+ y (* 1/22 n-hg))]
            [y1      (+ n-y (if (< hg1 hg2) (/ (- hg2 hg1) 2) 0))]
            [y2      (+ n-y (if (< hg2 hg1) (/ (- hg1 hg2) 2) 0))]
            [child1* (get-specs child1 n-x y1 norm)])
       (match-define (specs _ _ wd* _ _) (obtain-spcs child1*))
       (define n-x* (+ x wd* (* 2/23 n-wd)))
       (define child2* (get-specs child2 n-x* y2 norm))
       (bracket expr child1* child2*
                (specs x y n-wd n-hg color)))]))

;---

(define (draw-bgs dc node)
  (define spcs (obtain-spcs node))
  (match-define (specs x y width heigth color) spcs)
  (define childs (obtain-childs node))
  
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (list-ref colors color) 'solid)
  (define rect-path (draw-rounded-rectangle dc x y width heigth))
  (send dc draw-path rect-path)

  (when childs
    (for-each (draw-bgs dc _) childs)))

;---
;----- PLAY
;---

(send frame show #t)