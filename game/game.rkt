#lang racket/gui

(require "colors-and-sounds.rkt" "bitmaps.rkt" "../main.rkt" racket/draw)

;;; Abbreviations
; childs     -> children
; lambd      -> lambda expression
; var        -> variable
; argt       -> argtument
; wd         -> width
; hg         -> heigt
; norm       -> normalizer
; bg         -> background
; specs/spcs -> specifications

;---
;----- CANVAS SHAPE
;---

(define cv-width  800)
(define cv-height 600)

;---
;----- GUI
;---

(define frame
  (new frame%
       [label "Game"]))

(define congr-frame
  (new
   (class frame%
     (define/augment (on-close)
       (stop))
     (super-new [label "Congratulations!"]))))

(define congr-message
  (new message% [parent congr-frame]
       [label "Congratulations!"]
       [font  (make-object font% 30 'modern)]))

(define ops-frame
  (new frame%
       [label "Ops!"]))

(define ops-message
  (new message% [parent ops-frame]
       [label "Not Yet!"]
       [font  (make-object font% 30 'modern)]))

(define panel1
  (new horizontal-panel% [parent frame]))

(define message
  (new message% [parent panel1]
       [label "Hello!"]
       [stretchable-width #t]))

(define button-send
  (new button% [parent panel1]
       [label "Send Answer!"]
       [callback
        (λ _
          (cond
            [(send canvas check-answer)
             (play-sound 2)
             (send congr-frame show #t)]
            [else
             (send ops-frame show #t)]))]))

(define panel2
  (new horizontal-panel% [parent frame]))
 
(define text-field
  (new text-field% [parent panel2]
       [label "Lambda Expression: "]
       [init-value "(λ x . x x) (λ x . x)"]))

(define button-run
  (new button% [parent panel2]
       [label "Run!"]
       [callback
        (λ _
          (send canvas draw))]))

(define (send-expr expr)
  (define text  (format "Expression: ~a" expr))
  (define text* (substring text 0 (min (* 1/5 cv-width) (string-length text))))
  (send message set-label text*))

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
     (define expr          null)

     (define/public (draw)
       (define expr (get-expr))
       (when expr
         (reset-colors)
         (define node  (get-nodes expr))
         (define node* (get-var-colors node))
         (redraw node*)))

     (define/public (redraw node)
       (define node*   (get-sizes node))
       (define-values  (node** x y) (normalize-nodes node*))
       (define node*** (get-specs node** x y))
       
       (define dc (get-dc))
       (send dc set-background "DarkGray")
       (send dc clear)
       (send dc set-smoothing 'aligned)
       (draw-bgs dc node***)

       (set! expr (to-expr node))
       (set! nodes node***)
       (send-expr expr))

     (define/public (get-kinship event)
       (define node nodes)
       (define x (- (send event get-x) 15))
       (define y (- (send event get-y) 15))
       (and (in-this-bg? x y node)
            (match-node x y node)))
     
     (define/public (check-answer)
       (define expr* (α (eval-expr expr)))
       (equal? expr expr*))

     (define/override (on-event event)
       (when (send event button-down?)
         (define dc           (get-dc))
         (define curr-kinship (get-kinship event))
         
         (cond
           [(and curr-kinship (not (equal? kinship curr-kinship))) 
            (define node        nodes)
            (define rev-kinship (reverse curr-kinship))
            (define curr-node   (get-node-by-kinship node rev-kinship))
            (cond
              [kinship
               (match `(,kinship ,curr-kinship)
                 [`((argt left ,rest ...) (rigth ,rest ...))
                  (display-answer dc curr-node 'sucess)
                  (sleep/yield 1/2)
                  
                  (define way2node (reverse rest))
                  (define node*    (reduce node way2node))
                  (redraw node*)]
                 
                 [_
                  (display-answer dc selected-node 'error)
                  (sleep/yield 1/2)
                  (draw-bgs dc selected-node)])

               (set! kinship       #f)
               (set! selected-node #f)]
           
              [else
               (set! kinship       curr-kinship)
               (set! selected-node curr-node)
               (display-answer dc curr-node 'hit)])]
           
           [else
            (when kinship
              (set! kinship #f))
            (when selected-node
              (draw-bgs dc selected-node)
              (set! selected-node #f))])))
     
     (super-new
      [parent     frame]
      [min-width  (+ 30 cv-width)]
      [min-height (+ 30 cv-height)]
      [paint-callback
       (λ (_ dc)
         (send dc set-origin 15 15)
         (draw))]))))

;---
;----- GET ANSWER
;---

(define (get-eval-thread mode-eval expr result-thread)
  (thread
   (λ _
     (define expr* (mode-eval expr))
     (thread-send result-thread expr*))))

(define (eval-expr expr)
  (define curr-thread (current-thread))
  (define lazy-thread (get-eval-thread lazy-eval expr curr-thread))
  (sleep 1/3)
  (cond
    [(thread-running? lazy-thread)
     (kill-thread lazy-thread)
     expr]
    [else
     (define expr* (thread-receive))
     (define strict-thread (get-eval-thread strict-eval expr* curr-thread))
     (sleep 1/3)
     (cond 
       [(thread-running? strict-thread)
        (kill-thread strict-thread)
        expr*]
       [else
        (thread-receive)])]))

;---
;----- REPRESENTATION OF THE EXPRESSION AS NODES
;---

(struct specs    (x y width heigt color))
(struct var/argt (expr                spcs))
(struct lambd    (argt  child         spcs))
(struct bracket  (elder child1 child2 spcs))

;---

(define obtain-specs
  (match-lambda
    [(or (var/argt    _ spcs)
         (lambd     _ _ spcs)
         (bracket _ _ _ spcs))
     spcs]))

;---

(define (get-expr)
  (define text (send text-field get-value))
  (with-handlers ([exn:fail? invalid-expression])
    (α (rw-expr (string->expr text)) #f)))

(define (invalid-expression exn)
  (send message set-label "INVALIDE EXPRESSION!")
  #f)

;---

(define (get-nodes expr [parent-color 6])
  (define color (one-color-available parent-color))
  (define spcs  (specs 0 0 0 0 color))
  (match expr
    [(? symbol?)
     (var/argt expr spcs)]

    [`(λ ,arg ,body)
     (define argt  (var/argt arg spcs))
     (define child (get-nodes body color))
     (lambd argt child spcs)]

    [`(,expr1 ,expr2)
     (define elder  (var/argt #f (specs 0 0 0 0 5)))
     (define child1 (get-nodes expr1 color))
     (define child2 (get-nodes expr2 color))
     (bracket elder child1 child2 spcs)]))

;---

(define (get-var-colors node [hcolors (make-immutable-hash)])
  (match node
    [(var/argt expr (specs x y wd hg color))
     (define color* (if (hash-has-key? hcolors expr)
                        (hash-ref hcolors expr)
                        color))
     (var/argt expr (specs x y wd hg color*))]
    
    [(lambd argt child spcs)
     (match-define    (var/argt expr a-spcs)  argt)
     (define color*   (one-color-available))
     (define hcolors* (hash-set hcolors expr color*))
     (define argt*    (get-var-colors argt  hcolors*))
     (define child*   (get-var-colors child hcolors*))
     (lambd argt* child* spcs)]
    
    [(bracket elder child1 child2 spcs)
     (define child1* (get-var-colors child1 hcolors))
     (define child2* (get-var-colors child2 hcolors))
     (bracket elder child1* child2* spcs)]))

;---

(define get-sizes
  (match-lambda
    [(? var/argt? node)
     (get-size-var/argt node)]
    [(? lambd? node)
     (get-size-lambd node)]
    [(? bracket? node)
     (get-size-bracket node)]))

(define (get-size-var/argt node)
  (match-define (var/argt expr (specs _ _ _ _ color)) node)
  (var/argt expr (specs 0 0 90 100 color)))

(define (get-size-lambd node)
  (match-define       (lambd argt child (specs _ _ _ _ color)) node)
  (define child*      (get-sizes child))
  (define child-spcs  (obtain-specs child*))
  (match-define       (specs _ _ c-wd c-hg _) child-spcs)
  (define wd*         (* 22/20 (max 200 c-wd)))
  (define hg*         (* 23/20 (+ 125 c-hg)))
  (match-define       (var/argt a-expr (specs _ _ _ _ a-color)) argt)
  (define argt*       (var/argt a-expr (specs 0 0 200 100 a-color)))
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
  (define elder*      (var/argt e-expr (specs 0 0 180 100 e-color)))
  (bracket elder* child1* child2* (specs 0 0 wd* hg* color)))

;---

(define (normalize-nodes node)
  (define node-spcs (obtain-specs node))
  (match-define     (specs _ _ width height _) node-spcs)
  (define norm      (min (/ cv-width  width) (/ cv-height height)))
  (define x         (* 1/2 (- cv-width  (* norm width))))
  (define y         (* 1/2 (- cv-height (* norm height))))
  (define node*     (norm-node node norm))
  (values node* x y))

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

(define (norm-specs spcs norm)
  (match-define (specs x y wd hg color) spcs)
  (define norm-wd (* norm wd))
  (define norm-hg (* norm hg))
  (specs x y norm-wd norm-hg color))

;---
  
(define (get-specs node x y)
  (match node
    [(? var/argt?)
     (get-specs-var/argt node x y)]
    [(? lambd?)
     (get-specs-lambd node x y)]
    [(? bracket?)
     (get-specs-bracket node x y)]))

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

;---
;----- INTERACTION
;---

(define (match-node x y node [kinship null])
  (match node
    [(? var/argt?)
     kinship]
    
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

(define (in-this-bg? x y node)
  (define spcs (obtain-specs node))
  (match-define (specs nd-x nd-y wd hg _) spcs)
  (and (< nd-x x (+ nd-x wd))
       (< nd-y y (+ nd-y hg))))

;---

(define (get-node-by-kinship node kinship)
  (cond
    [(null? kinship) node]
    [else
     (define rst-kin (cdr kinship))
     (match (car kinship)
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
;----- ANIMATION
;---
    
(define (display-answer dc node color)
  (define color*
    (match color
      ['hit
       6]
      ['sucess
       (play-sound 1)
       6]
      ['error
       (play-sound 0)
       7]
      [_ color]))
  (draw-bg dc node color*))

;---

(define (draw-bgs dc node)
  (match node
    [(var/argt _ _)
     (draw-bg dc node #:figure 'egg)]
    [(lambd argt child _)
     (draw-bg dc node)
     (draw-bg dc argt #:figure 'trex)
     (draw-bgs dc child)]
    [(bracket elder child1 child2 _)
     (draw-bg dc node)
     (draw-bg dc elder #:figure 'triceratops)
     (draw-bgs dc child1)
     (draw-bgs dc child2)]))

(define (draw-bg dc node [color1 #f] #:figure [figure #f])
  (define spcs    (obtain-specs node))
  (match-define   (specs x y width height color2) spcs)
  (define color*  (cond [color1] [color2]))
  (define color** (get-color color*))
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush color** 'solid)
  (draw-rounded-rectangle dc x y width height)
  (when figure
    (define bitmap (bitmap-figure figure))
    (define scale  (* width (match figure
                              ['trex 1/310]
                              ['triceratops 1/1200]
                              ['egg 1/1500])))
    (define wd*    (/ width  scale))
    (define hg*    (/ height scale))
    (define x*     (+ (/ x scale) (* 1/2 (- wd* (send bitmap get-width)))))
    (define y*     (+ (/ y scale) (* 1/2 (- hg* (send bitmap get-height)))))
    (send dc set-scale scale scale)
    (send dc draw-bitmap bitmap x* y*)
    (send dc set-scale 1 1)))

;---

(define (draw-rounded-rectangle dc x y width height)
  (define path (new dc-path%))
  (define (curve-to x1 y1 x2 y2 x3 y3)
    (send path curve-to x1 y1 x2 y2 x3 y3))
  (send path move-to 0 0.1)
  (curve-to 0     0.05  0.05  0     0.1   0)
  (curve-to 0.2   -0.02 0.8   -0.02 0.9   0)
  (curve-to 0.95  0     1     0.05  1     0.1)
  (curve-to 1.02  0.2   1.02  0.8   1     0.9)
  (curve-to 1     0.95  0.95  1     0.9   1)
  (curve-to 0.8   1.02  0.2   1.02  0.1   1)
  (curve-to 0.05  1     0     0.95  0     0.9)
  (curve-to -0.02 0.8   -0.02 0.2   0     0.1)
  (send path scale width height)
  (send path translate x y)
  (send dc draw-path path))

;---

(define (to-expr node)
  (define to-expr*
    (match-lambda
      [(var/argt expr _)
       expr]

      [(lambd (var/argt arg _) child spcs)
       (define body (to-expr* child))
       `(λ ,arg ,body)]

      [(bracket _ child1 child2 _)
       (define expr1 (to-expr* child1))
       (define expr2 (to-expr* child2))
       `(,expr1 ,expr2)]))
  
  (define expr (α (to-expr* node)))
  expr)

;---
;----- REDUCTION
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
        (bracket elder child1* child2 spcs)]
       
       ['rigth
        (match-define (bracket elder child1 child2 spcs) node)
        (define child2* (reduce child2 rst-kin))
        (bracket elder child1 child2* spcs)])]))
;---

(define (node-replace node)
  (match-define (bracket _ (lambd (var/argt from _) body _) to _) node)
  (define to*   (alpha to))
  (define to**  (get-var-colors to*))
  (define lex-rplc
    (match-lambda
      [(var/argt (? (λ (val) (equal? from val))) _)
       to**]

      [(? var/argt? var)
       var]

      [(lambd argt child spcs)
       (define child* (lex-rplc child))
       (lambd argt child* spcs)]

      [(bracket elder child1 child2 spcs)
       (define child1* (lex-rplc child1))
       (define child2* (lex-rplc child2))
       (bracket elder child1* child2* spcs)]))
  (lex-rplc body))

(define (alpha node [hargts (make-immutable-hash)])
  (match node
    [(var/argt expr spcs)
     (define expr* (if (hash-has-key? hargts expr)
                       (hash-ref hargts expr)
                       expr))
     (var/argt expr* spcs)]
     
    [(lambd (var/argt expr a-spcs) child spcs)
     (define ?arg    (get-?arg))
     (define hargts* (hash-set hargts expr ?arg))
     (define child*  (alpha child hargts*))
     (lambd (var/argt ?arg a-spcs) child* spcs)]

    [(bracket elder child1 child2 spcs)
     (define child1* (alpha child1 hargts))
     (define child2* (alpha child2 hargts))
     (bracket elder child1* child2* spcs)]))

;---
;----- PLAY
;---

(send frame show #t)

;---