#lang racket/gui

(require fancy-app racket/draw
         (only-in "main.rkt" string->expr α rw-expr rev-rw lst-defs))

;---

(define width          800)
(define heigth         600)
(define expression     null)
(define family-posit   0)
(define families-width (make-hash))
(define kinship        (make-hash))
(define colors
  (list
   (make-object color% 225 169 84  1/2)
   (make-object color% 178 139 100 1/2)
   (make-object color% 98  116 93  1/2)
   (make-object color% 36  139 64  1/2)
   (make-object color% 1   75  13  1/2)))
(struct family-coord (coord children))

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
  (set! family-posit   0)
  (get-family-specs expression)
  (void))

;---

(define (get-color father-color)          
  (define available-colors
    (remove father-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

;---

(define (get-families x y scale family-posit)
  (family-coord
   (list x y scale)
   (if (hash-has-key? kinship family-posit)
       (let* ([children    (hash-ref kinship family-posit)]
              [family-size (add1 (length children))]
              [space       (if (> family-size 2)
                               (/ (* 1/11 scale) family-size)
                               (/ scale 4))]
              [spaces      0])
         (for/list ([child (in-list children)])
           (define family-width (hash-ref families-width child))
           (set! spaces (+ space spaces family-width))
           (get-families (- (+ spaces x) family-width)
                         (- (+ (/ scale 2) y) (/ family-width 2))
                         family-width
                         child)))
       null)))

(define (draw-backgrounds dc family)
  (let loop ([father-color 4]
             [family       family])
    (define color (get-color father-color))
    (match-define (family-coord (list x y scale) children) family)
    (send dc set-pen "DarkGray" 1 'hilite)
    (send dc set-brush (list-ref colors color) 'solid)
    (send dc draw-rounded-rectangle x y scale scale)
    (for-each (loop color _) children)))

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
       (let* ([root-width   (hash-ref families-width 0)]
              [scale-width  (/ width root-width)]
              [scale-heigth (/ heigth root-width)]
              [dc           (get-dc)])
         (send dc set-background "LightGray")
         (send dc clear)
         (send dc set-scale scale-width scale-heigth)
         (define family (get-families 0 0 root-width 0))
         (draw-backgrounds dc family)))

     (define/public (draw)
       (when (get-expr)
         (get-families-specs)
         (redraw)))
      
     (super-new
       [parent         frame]
       [min-width      width]
       [min-height     heigth]
       [paint-callback (λ _ (draw))]))))

;---

(send frame show #t)

;---