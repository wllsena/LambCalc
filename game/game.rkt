#lang racket/gui

(require "../main.rkt" fancy-app racket/draw rsound)

;;; Abbreviations
; rlt    -> relative
; childs -> children
; lambd  -> lambda expression
; var    -> variable
; arg    -> argument (number)
; argt   -> argument (struct)
; wd     -> width
; hg     -> heigt
; norm   -> normalizer
; bg     -> background
; specs  -> specifications
; dif    -> difference

;---

(define cv-width  800) ; +30
(define cv-heigth 600) ; +30

;---

(define sounds
  (list
   (rs-read "sounds/error_sound.wav"))) ;0 error-sound

;---

(define colors
  (list
   (make-object color% 225 169 84)        ;0
   (make-object color% 178 139 100)       ;1
   (make-object color% 98  116 93)        ;2
   (make-object color% 36  139 64)        ;3
   (make-object color% 1   75  13)        ;4
   (make-object color% 128 128 128 1/2)   ;5 
   (make-object color% 75  181 67  3/4)   ;6 red-error
   (make-object color% 192 32  32  3/4))) ;7 green-sucess

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
       [callback
        (λ _
          (send message set-label "Hello")
          (send canvas draw))]))

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
       (define node*     (get-sizes node))
       (normalize-nodes node*)
       (define node-spcs (obtain-specs node*))
       (match-define     (specs _ _ root-wd root-hg _) node-spcs)
       (define norm      (min (/ cv-width  root-wd)
                              (/ cv-heigth root-hg)))
       (define x         (* 1/2 (- cv-width  (* norm root-wd))))
       (define y         (* 1/2 (- cv-heigth (* norm root-hg))))
       (define node**    (get-specs node* x y norm))
       
       (define dc (get-dc))
       (send dc set-background "DarkGray")
       (send dc clear)
       (send dc set-smoothing 'aligned)
       (draw-bgs dc node**))

     (super-new
       [parent     frame]
       [min-width  (+ 30 cv-width)]
       [min-height (+ 30 cv-heigth)]
       [paint-callback
        (λ (_ dc)
          (send dc set-origin 15 15)
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

(struct specs    (x y width heigt color))
(struct var/arg  (expr                     spcs))
(struct lambd    (expr argt  child         spcs))
(struct bracket  (expr elder child1 child2 spcs))

;---

(define (one-color-available parent-color)          
  (define available-colors
    (remove parent-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

;---

(define (get-nodes expr [parent-color 6])
  (define color (one-color-available parent-color))
  (define spcs  (specs 0 0 0 0 color))
  (match expr
    [(? symbol?)
     (var/arg expr spcs)]

    [`(λ ,arg ,body)
     (define argt  (var/arg arg (specs 0 0 0 0 5)))
     (define child (get-nodes body color))
     (lambd expr argt child spcs)]

    [`(,expr1 ,expr2)
     (define elder  (var/arg #f (specs 0 0 0 0 5)))
     (define child1 (get-nodes expr1 color))
     (define child2 (get-nodes expr2 color))
     (bracket expr elder child1 child2 spcs)]))

;---

(define obtain-specs
  (match-lambda
    [(or (var/arg       _ spcs)
         (lambd     _ _ _ spcs)
         (bracket _ _ _ _ spcs))
     spcs]))

;---

(define (get-size-var/arg node [arg? #f])
  (match-define (var/arg expr (specs _ _ _ _ color)) node)
  (var/arg expr (specs 0 0 (if arg? 100 200) 100 color)))

(define (get-size-lambd node)
  (match-define (lambd expr argt child (specs _ _ _ _ color)) node)
  (define argt*      (get-size-var/arg argt))
  (define child*     (get-sizes child))
  (define child-spcs (obtain-specs child*))
  (match-define      (specs _ _ wd hg _) child-spcs)
  (define wd*        (* 22/20 (max 200 wd)))
  (define hg*        (* 23/20 (+ 100 hg)))
  (define spcs       (specs 0 0 wd* hg* color))
  (lambd expr argt* child* spcs))

(define (get-size-bracket node)
  (match-define (bracket expr elder child1 child2 (specs _ _ _ _ color)) node)
  (define elder*      (get-size-var/arg elder))
  (define child1*     (get-sizes child1))
  (define child2*     (get-sizes child2))
  (define child1-spcs (obtain-specs child1*))
  (define child2-spcs (obtain-specs child2*))
  (match-define       (specs _ _ wd1 hg1 _) child1-spcs)
  (match-define       (specs _ _ wd2 hg2 _) child2-spcs)  
  (define wd*         (* 23/20 (+ wd1 wd2)))
  (define hg*         (* 23/20 (+ 100 (max hg1 hg2))))
  (define spcs        (specs 0 0 wd* hg* color))
  (bracket expr elder* child1* child2* spcs))
     
(define get-sizes
  (match-lambda
    [(? var/arg? node)
     (get-size-var/arg node #t)]
    [(? lambd? node)
     (get-size-lambd node)]
    [(? bracket? node)
     (get-size-bracket node)]))

;---

(define (norm-specs spcs norm)
  (match-define (specs x y wd hg color) spcs)
  (define norm-wd (* norm wd))
  (define norm-hg (* norm hg))
  (specs x y norm-wd norm-hg color))

;---

(define (norm-node node norm)
  (match node
    [(var/arg expr spcs)
     (define spcs* (norm-specs spcs norm))
     (var/arg expr spcs*)]
    [(lambd expr argt child spcs)
     (define argt*  (norm-node argt norm))
     (define child* (norm-node child norm))
     (define spcs*  (norm-specs spcs norm))
     (lambd expr argt* child* spcs*)]
    [(bracket expr elder child1 child2 spcs)
     (define elder*  (norm-node elder norm))
     (define child1* (norm-node child1 norm))
     (define child2* (norm-node child2 norm))
     (define spcs*   (norm-specs spcs norm))
     (bracket expr elder* child1* child2* spcs*)]))

(define (normalize-nodes node)
  (define node-spcs (obtain-specs node))
  (match-define     (specs _ _ width heigth _) node-spcs)
  (define norm      (min (/ cv-width  width) (/ cv-heigth heigth)))
  (define x         (* 1/2 (- cv-width  (* norm width))))
  (define y         (* 1/2 (- cv-heigth (* norm heigth))))
  (define node*     (norm-node node norm))
  (values x y node*))

  
#|
(define (get-specs-var/arg node x y norm) (void))

(define get-specs
  (match-lambda
    [(? var/arg? node)
     (get-specs-var/arg node)]
    [(? lambd? node)
     (get-specs-lambd node)]
    [(? bracket? node)
     (get-specs-bracket node)]))
|#
;---

(define (get-specs node x y norm)
  (match node
    [(var/arg expr (specs _ _ wd hg color))
     (var/arg expr (specs x y (* norm wd) (* norm hg) color))]

    [(lambd expr argt child (specs _ _ wd hg color))
     (match-define (specs _ _ _ (app (* norm _) c-hg) _)
       (obtain-specs child))
     (let* ([n-wd   (* norm wd)]
            [n-hg   (* norm hg)]
            [n-x    (+ x (if (< c-hg (* 200 norm))
                             (/ (- n-hg c-hg) 2)
                             (* 1/22 n-wd)))]
            [n-y    (+ y (* 100 norm) (* 2/23 n-hg))]
            [child* (get-specs child n-x n-y norm)])
       (match-define (var/arg arg (specs _ _ a-wd a-hg arg-color)) argt)
       (define n-a-wd (+ x (/ (- n-wd (* norm a-wd)) 2)))
       (define n-a-hg (+ y (* 1/23 n-hg)))
       (define argt*  (var/arg arg (specs n-a-wd n-a-hg
                                          (* norm a-wd) (* norm a-hg)
                                          arg-color)))
       (lambd expr argt* child* (specs x y n-wd n-hg color)))]

    [(bracket expr elder child1 child2 (specs _ _ wd hg color))
     (define n-wd (* norm wd))
     (define n-hg (* norm hg))
     (define n-y  (+ y (* 100 norm) (* 2/23 n-hg)))
     (define n-x (+ x (* 1/23 n-wd)))
     (match-define (specs _ _ _ (app (* norm _) hg1) _)
       (obtain-specs child1))
     (match-define (specs _ _ _ (app (* norm _) hg2) _)
       (obtain-specs child2))
     (define y1 (+ n-y (if (< hg1 hg2) (/ (- hg2 hg1) 2) 0)))
     (define y2 (+ n-y (if (< hg2 hg1) (/ (- hg1 hg2) 2) 0)))
     (define child1* (get-specs child1 n-x y1 norm))
          
     (match-define (specs _ _ wd* _ _) (obtain-specs child1*))
     (define n-x* (+ x wd* (* 2/23 n-wd)))
     (define child2* (get-specs child2 n-x* y2 norm))
     (bracket expr elder child1* child2* (specs x y n-wd n-hg color))]))

;---

(define (draw-bg dc node)
  (define spcs  (obtain-specs node))
  (match-define (specs x y width heigth color) spcs)
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (list-ref colors color) 'solid)
  (define rect-path (draw-rounded-rectangle dc x y width heigth))
  (send dc draw-path rect-path))
  
(define (draw-bgs dc node)
  (draw-bg dc node)
  
  (match node
    [(var/arg _ _)
     (void)]
    [(lambd _ argt child _)
     (draw-bg dc argt)
     (draw-bgs dc child)]
    [(bracket _ elder child1 child2 _)
     (draw-bg dc elder)
     (draw-bgs dc child1)
     (draw-bgs dc child2)]))

;---
;----- PLAY
;---

(send frame show #t)

;---