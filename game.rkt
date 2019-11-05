#lang racket

(require racket/gui racket/draw
         (only-in "main.rkt" string->expr α rw-expr rev-rw lst-defs))

;---

(define colors
  (hash
   'green  (make-object color% 53  212 97  1/2)
   'yellow (make-object color% 249 225 4   1/2)
   'orange (make-object color% 249 157 7   1/2)
   'violet (make-object color% 136 47  246 1/2)
   'blue   (make-object color% 55  182 246 1/2)))

;--- 

(define families-width (make-hash))
(define kinship        (make-hash))
(define family-posit   0)
(define expression     '())

;----

(define (get-expr)
  (define text (send text-field get-value))
  (with-handlers ([exn:fail?
                   (λ (exn)
                     (send message set-label "INVALIDE EXPRESSION!")
                     #f)])
    (set! expression (rev-rw (α (rw-expr (string->expr text)))))))

(define (get-family-specs expr)
  (define curr-posit family-posit)
  (set! family-posit (add1 curr-posit))
  (define family-width
    (match expr
      [(? symbol?) 100]
      [(or `(λ ,_ ,body ...)
           `(,body ...))
       (define children (map get-family-specs body))
       (hash-set! kinship curr-posit children)
       (max 200 (for/sum ([child (in-list children)])
                  (hash-ref families-width child)))]))
  (hash-set! families-width curr-posit (* 11/10 family-width))
  curr-posit)

(define (get-families-specs)
  (set! families-width (make-hash))
  (set! kinship        (make-hash))
  (set! family-posit  0)
  (get-family-specs expression)
  (void))

;---

(define (get-color father-color)
  (define available-colors
    (remove father-color '(green yellow orange violet blue)))
  (list-ref available-colors (random 4)))

;---

(define (draw-family dc x y family-posit scale father-color)
  (define color (get-color father-color))
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (hash-ref colors color) 'solid)
  (send dc draw-rounded-rectangle x y scale scale)

  (when (hash-has-key? kinship family-posit)
    (define children    (hash-ref kinship family-posit))
    (define family-size (add1 (length children)))
    (define corridor    (if (> family-size 2)
                            (/ (* 1/11 scale) family-size)
                            (/ scale 4)))
    (for/fold ([coord corridor])
              ([child (in-list children)])
      (define family-width (hash-ref families-width child))
      (draw-family dc (+ coord x)
                   (- (+ (/ scale 2) y) (/ family-width 2))
                   child family-width color)
      (+ corridor coord family-width))))

;--- Frame

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
       [init-value "(λ x . x) (λ y . y)"]))

(define button
  (new button% [parent panel]
       [label "Go"]
       [callback (λ _ (send canvas draw))]))

;--- Canvas

(define canvas
  (new
   (class canvas%
     (inherit get-dc)
   
     (define/override (on-event evt)
       (cond
         [(send evt button-up?)]
         [(send evt button-down?)]
         [(send evt dragging?)]
         [(send evt moving?)]))
     
     (define/public (redraw)
       (let* ([root-width (hash-ref families-width 0)]
              [scale      (/ 1000 root-width)]
              [dc         (get-dc)])
         (send dc set-background "DarkGray")
         (send dc clear)
         (send dc set-scale scale scale)
         (draw-family dc 0 0 0 root-width 'DarkGray)))

     (define/public (draw)
       (when (get-expr)
         (get-families-specs)
         (redraw)))
      
     (super-new
      [parent         frame]
      [min-width      1000]
      [min-height     1000]
      [paint-callback (λ _ (draw))]))))

;---

(send frame show #t)

;---