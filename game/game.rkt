#lang racket/gui

(require "../main.rkt" fancy-app racket/draw rsound)

;;; Abbreviations
; rlt    -> relative
; childs -> children
; lambd  -> lambda expression
; var    -> variable
; arg    -> argtument (number)
; wd     -> width
; hg     -> heigt
; norm   -> normalizer
; bg     -> background
; specs  -> specifications
; dif    -> difference

;---

(define cv-width  800)
(define cv-heigth 600)

;---

(define sounds
  (list
   (rs-read "sounds/error_sound.wav"))) ;0 error-sound

;---

(define bg-colors
  (list
   (make-color 225 169 84)        ;0
   (make-color 178 139 100)       ;1
   (make-color 98  116 93)        ;2
   (make-color 36  139 64)        ;3
   (make-color 1   75  13)        ;4
   (make-color 128 128 128 1/2)   ;5 
   (make-color 75  181 67  3/4)   ;6 green-sucess
   (make-color 192 32  32  3/4))) ;7 red-error

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

     (define nodes         null)
     (define kinship       #f)
     (define selected-node #f)

     (define/public (draw)
       (define expr (get-expr))
       (when expr
         (define node (get-nodes expr))
         (redraw node)))

     (define/public (redraw node)
       (define node*   (get-sizes node))
       (define-values  (node** x y) (normalize-nodes node*))
       (define node*** (get-specs node** x y))
       (set! nodes node***)
       
       (define dc (get-dc))
       (send dc set-background "DarkGray")
       (send dc clear)
       (send dc set-smoothing 'aligned)
       (draw-bgs dc node***))

     (define/public (get-kinship event)
       (define node nodes)
       (define x (send event get-x))
       (define y (send event get-y))
       (and (in-this-bg? x y node)
            (match-node x y node)))

     (define/override (on-event event)
       (when (send event button-down?)
         (define dc           (get-dc))
         (define curr-kinship (get-kinship event))
         
         (cond
           [curr-kinship
            (cond
              [kinship
               (match `(,kinship ,curr-kinship)
                 [`((argt left ,rest ...) (rigth ,rest ...))
                  (display-answer dc selected-node 6)
                  (sleep/yield 1/2)
                  
                  (define rev-kinship (reverse rest))
                  (define node (reduce nodes rev-kinship))
                  (redraw node)]
                 
                 [_
                  (display-answer dc selected-node 7)
                  (sleep/yield 1/2)
                  (draw-bgs dc selected-node)])

               (set! kinship       #f)
               (set! selected-node #f)]
           
              [else
               (set! kinship curr-kinship)
               (define rev-kinship (reverse curr-kinship))
               (set! selected-node (get-node-by-kinship nodes rev-kinship))
               (display-answer dc selected-node 6)])]
           
           [else
            (when selected-node
              (draw-bgs dc selected-node)
              (set! selected-node #f))
            (set! kinship #f)])))
     
     (super-new
       [parent     frame]
       [min-width  cv-width]
       [min-height cv-heigth]
       [paint-callback
        (λ (_ dc)
          (draw))]))))

;---
    
(define (display-answer dc node color)
  (when (= color 7)
    (play (list-ref sounds 0)))
  (draw-bg dc node color))

;---

(define (reduce node kinship)
  (cond
    [(null? kinship)
     (node-replace node)]
    [else
     (define rst-kin (cdr kinship))
     (match (car kinship)
       ['child
        (match-define (lambd argt child spcs) node)
        (define child* (reduce child rst-kin))
        (lambd argt child* spcs)]
       
       ['left
        (match-define (bracket elder child1 child2 spcs) node)
        (define child1* (reduce child1 rst-kin))
        (elder child1* child2 spcs)]
       
       ['rigth
        (match-define (bracket elder child1 child2 spcs) node)
        (define child2* (reduce child2 rst-kin))
        (elder child1 child2* spcs)])]))
;---

(define (node-replace node)
  ;(match-define 
  node)

;---

(define (get-node-by-kinship node kinship)
  (cond
    [(null? kinship) node]
    [else
     (define rst-kin (cdr kinship))
     (match (car kinship)
       ['var node]
       ['argt
        (define argt (lambd-argt node))
        (get-node-by-kinship argt rst-kin)]
       ['child
        (define child (lambd-child node))
        (get-node-by-kinship child rst-kin)]
       ['elder
        (define elder (bracket-elder node))
        (get-node-by-kinship elder rst-kin)]
       ['left
        (define left (bracket-child1 node))
        (get-node-by-kinship left rst-kin)]
       ['rigth
        (define rigth (bracket-child2 node))
        (get-node-by-kinship rigth rst-kin)])]))

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
(struct var/argt (expr                spcs))
(struct lambd    (argt  child         spcs))
(struct bracket  (elder child1 child2 spcs))

;---

(define (one-color-available parent-color)          
  (define available-colors
    (remove parent-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

;---

(define (get-nodes expr [parent-color 6]) ; MUDAR
  (define color (one-color-available parent-color))
  (define spcs  (specs 0 0 0 0 color))
  (match expr
    [(? symbol?)
     (var/argt expr spcs)]

    [`(λ ,arg ,body)
     (define argt  (var/argt arg (specs 0 0 0 0 5)))
     (define child (get-nodes body color))
     (lambd argt child spcs)]

    [`(,expr1 ,expr2)
     (define elder  (var/argt #f (specs 0 0 0 0 5)))
     (define child1 (get-nodes expr1 color))
     (define child2 (get-nodes expr2 color))
     (bracket elder child1 child2 spcs)]))

;---

(define obtain-specs
  (match-lambda
    [(or (var/argt     _ spcs)
         (lambd     _ _ spcs)
         (bracket _ _ _ spcs))
     spcs]))

;---

(define (get-size-var/argt node)
  (match-define (var/argt expr (specs _ _ _ _ color)) node)
  (var/argt expr (specs 0 0 100 100 color)))

(define (get-size-lambd node)
  (match-define (lambd argt child (specs _ _ _ _ color)) node)
  (define child*     (get-sizes child))
  (define child-spcs (obtain-specs child*))
  (match-define      (specs _ _ c-wd c-hg _) child-spcs)
  (define wd*        (* 22/20 (max 200 c-wd)))
  (define hg*        (* 23/20 (+ 100 c-hg)))
  (match-define      (var/argt a-expr (specs _ _ _ _ a-color)) argt)
  (define argt*      (var/argt a-expr (specs 0 0 200 100 a-color)))
  (lambd argt* child* (specs 0 0 wd* hg* color)))

(define (get-size-bracket node)
  (match-define (bracket elder child1 child2 (specs _ _ _ _ color)) node)
  (define child1*     (get-sizes child1))
  (define child2*     (get-sizes child2))
  (define child1-spcs (obtain-specs child1*))
  (define child2-spcs (obtain-specs child2*))
  (match-define       (specs _ _ c1-wd c1-hg _) child1-spcs)
  (match-define       (specs _ _ c2-wd c2-hg _) child2-spcs)  
  (define wd*         (* 23/20 (+ c1-wd c2-wd)))
  (define hg*         (* 23/20 (+ 100 (max c1-hg c2-hg))))
  (match-define       (var/argt e-expr (specs _ _ _ _ e-color)) elder)
  (define elder*      (var/argt e-expr (specs 0 0 200 100 e-color)))
  (bracket elder* child1* child2* (specs 0 0 wd* hg* color)))
     
(define get-sizes
  (match-lambda
    [(? var/argt? node)
     (get-size-var/argt node)]
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
    [(var/argt expr spcs)
     (define spcs* (norm-specs spcs norm))
     (var/argt expr spcs*)]
    [(lambd argt child spcs)
     (define argt*  (norm-node argt norm))
     (define child* (norm-node child norm))
     (define spcs*  (norm-specs spcs norm))
     (lambd argt* child* spcs*)]
    [(bracket elder child1 child2 spcs)
     (define elder*  (norm-node elder norm))
     (define child1* (norm-node child1 norm))
     (define child2* (norm-node child2 norm))
     (define spcs*   (norm-specs spcs norm))
     (bracket elder* child1* child2* spcs*)]))

(define (normalize-nodes node)
  (define node-spcs (obtain-specs node))
  (match-define     (specs _ _ width heigth _) node-spcs)
  (define norm      (min (/ cv-width  width) (/ cv-heigth heigth)))
  (define x         (* 1/2 (- cv-width  (* norm width))))
  (define y         (* 1/2 (- cv-heigth (* norm heigth))))
  (define node*     (norm-node node norm))
  (values node* x y))

;---

(define (get-specs-var/argt node x y)
  (match-define (var/argt expr (specs _ _ wd hg color)) node)
  (var/argt expr (specs x y wd hg color)))

(define (get-specs-lambd node x y)
  (match-define (lambd argt child (specs _ _ wd hg color)) node)
  
  (define argt-spcs  (obtain-specs argt))
  (match-define      (specs _ _ a-wd a-hg _) argt-spcs)
  (define a-x        (+ x (* 1/2 (- wd a-wd))))
  (define a-y        (+ y (* 1/20 hg)))
  (define argt*      (get-specs-var/argt argt a-x a-y))
  
  (define child-spcs (obtain-specs child))
  (match-define      (specs _ _ c-wd c-hg _) child-spcs)
  (define c-x        (+ x (* 1/2 (- wd c-wd))))
  (define c-y        (+ y (* 2/20 hg) a-hg))
  (define child*     (get-specs child c-x c-y))
  
  (lambd argt* child* (specs x y wd hg color)))

(define (get-specs-bracket node x y)
  (match-define (bracket elder child1 child2 (specs _ _ wd hg color)) node)
  
  (define elder-spcs (obtain-specs elder))
  (match-define      (specs _ _ e-wd e-hg _) elder-spcs)
  (define e-x        (+ x (* 1/2 (- wd e-wd))))
  (define e-y        (+ y (* 1/20 hg)))
  (define elder*     (get-specs-var/argt elder e-x e-y))

  (define spaces-y    (+ e-hg (* 2/20 hg)))
  
  (define child1-spcs (obtain-specs child1))
  (match-define       (specs _ _ c1-wd c1-hg _) child1-spcs)
  (define c1-x        (+ x (* 1/20 wd)))
  (define c1-y        (+ y spaces-y (* 1/2 (- hg c1-hg spaces-y))))
  (define child1*     (get-specs child1 c1-x c1-y))

  (define child2-spcs (obtain-specs child2))
  (match-define       (specs _ _ c2-wd c2-hg _) child2-spcs)
  (define c2-x        (+ c1-x c1-wd (* 1/20 wd)))
  (define c2-y        (+ y spaces-y (* 1/2 (- hg c2-hg spaces-y))))
  (define child2*     (get-specs child2 c2-x c2-y))

  (bracket elder* child1* child2* (specs x y wd hg color)))
  
(define (get-specs node x y)
  (match node
    [(? var/argt?)
     (get-specs-var/argt node x y)]
    [(? lambd?)
     (get-specs-lambd node x y)]
    [(? bracket?)
     (get-specs-bracket node x y)]))

;---

(define (draw-bg dc node [color #f])
  (define spcs  (obtain-specs node))
  (match-define (specs x y width heigth color*) spcs)
  (define color** (cond [color] [color*]))
  
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (list-ref bg-colors color**) 'solid)
  (define rect-path (draw-rounded-rectangle dc x y width heigth))
  (send dc draw-path rect-path))
  
(define (draw-bgs dc node)
  (draw-bg dc node)
  (match node
    [(var/argt _ _)
     (void)]
    [(lambd argt child _)
     (draw-bg dc argt)
     (draw-bgs dc child)]
    [(bracket elder child1 child2 _)
     (draw-bg dc elder)
     (draw-bgs dc child1)
     (draw-bgs dc child2)]))

;---

(define (in-this-bg? x y node)
  (define spcs (obtain-specs node))
  (match-define (specs nd-x nd-y wd hg _) spcs)
  (and (< nd-x x (+ nd-x wd))
       (< nd-y y (+ nd-y hg))))

(define (match-node x y node [kinship null])
  (match node
    [(? var/argt?)
     (cons 'var kinship)]
    
    [(lambd argt child _)
     (cond
       [(in-this-bg? x y argt)
        (cons 'argt kinship)]
       [(in-this-bg? x y child)
        (define kinship* (cons 'child kinship))
        (match-node x y child kinship*)]
       [else
        kinship])]
    
    [(bracket elder child1 child2 _)
     (cond
       [(in-this-bg? x y elder)
        (cons 'elder kinship)]
       [(in-this-bg? x y child1)
        (define kinship* (cons 'left kinship))
        (match-node x y child1 kinship*)]
       [(in-this-bg? x y child2)
        (define kinship* (cons 'rigth kinship))
        (match-node x y child2 kinship*)]
       [else
        kinship])]))

;---


;---
;----- PLAY
;---

(send frame show #t)

;---