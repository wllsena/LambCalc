#lang racket/gui

(require fancy-app racket/draw
         (only-in "main.rkt" string->expr α rw-expr rev-rw lst-defs))

;---

(define width  1000)
(define heigth 1000)

;---

(struct kin (vars children))
(define colors
  (list
   (make-object color% 225 169 84)
   (make-object color% 178 139 100)
   (make-object color% 98  116 93)
   (make-object color% 36  139 64)
   (make-object color% 1   75  13)
   (make-object color% 166 50  50 3/4)))

;----

(define expression      null)
(define families-width  (make-hash))
(define families-heigth (make-hash))
(define kinship         (make-hash))
(define family-posit    0)

(define families-coord  (make-hash))
(define families-color  (make-hash))

;----

(define (failed-to-get-expr exn)
  (send message set-label "INVALIDE EXPRESSION!")
  #f)

(define (get-expr)
  (define text (send text-field get-value))
  (with-handlers ([exn:fail? failed-to-get-expr])
    (set! expression (rev-rw (α (rw-expr (string->expr text)))))))

;---

(define (get-family-dimensions expr curr-posit)
  (match expr
    [(? symbol?)
     (hash-set! kinship curr-posit #f)
     (values 100 100)]
    [(or `(λ (,var ...) ,body ...)
         `(,body ... ,var ...))
     (define children (map get-family-specs body))
     (hash-set! kinship curr-posit (kin var children))
     (values
      (max 200 (for/sum ([child (in-list children)])
                 (hash-ref families-width child)))
      (+ (* 100 (max 1 (length var)))
         (apply max (for/list ([child (in-list children)])
                      (hash-ref families-heigth child)))))]))

(define (get-family-specs expr)
  (define curr-posit family-posit)
  (set! family-posit (add1 curr-posit))
  (define-values (family-width family-heigth)
    (get-family-dimensions expr curr-posit))
  (hash-set! families-width  curr-posit (* 11/10 family-width))
  (hash-set! families-heigth curr-posit (* 11/10 family-heigth))
  curr-posit)

(define (get-families-specs)
  (set! families-width  (make-hash))
  (set! families-heigth (make-hash))
  (set! kinship         (make-hash))
  (set! family-posit    0)
  (get-family-specs expression)
  (void))

;---

(define (get-color father-color)          
  (define available-colors
    (remove father-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

;---
#|
(define (get-family-coord2 x y width heigth family-posit)
  (define coord (list x y width heigth))
  (define children
    (when (hash-has-key? kinship family-posit)
      (match-define (kin vars children) (hash-ref kinship family-posit))
      (let* ([family-len (add1 (length children))]
             [vars-len   (length vars)]
             [space-w    (if (> family-len 2)
                             (/ (* 1/11 width) family-len)
                             (/ width 4))]
             [spaces-w   0]
             [spaces-h   (+ y (/ (+ heigth (* 110 (length vars))) 2))])
        (for/list ([child (in-list children)])
          (define family-width  (hash-ref families-width  child))
          (define family-heigth (hash-ref families-heigth child))
          (set! spaces-w (+ spaces-w space-w family-width))
          (get-family-coord (- (+ spaces-w x) family-width)
                            (- spaces-h (/ family-heigth 2))
                            family-width
                            family-heigth
                            child)))))
  (family-coord coord (if (void? children) null children)))
|#
(define (get-families-coord x y width heigth [curr-posit 0])
  (hash-set! families-coord curr-posit (list x y width heigth))
  (define curr-kin (hash-ref kinship curr-posit))
  (when curr-kin
    (match-define (kin vars children) curr-kin)
    (let* ([family-len (add1 (length children))]
           [space-w    (if (> family-len 2)
                           (/ (* 1/11 width) family-len)
                           (/ width 4))]
           [spaces-w   0]
           [spaces-h   (+ y (/ (+ heigth (* 110 (length vars))) 2))])
      (for/fold ([spaces-w space-w])
                ([child    (in-list children)])
        (define family-width  (hash-ref families-width  child))
        (define family-heigth (hash-ref families-heigth child))
        (get-families-coord (+ spaces-w x) (- spaces-h (/ family-heigth 2))
                            family-width family-heigth child)
        (+ spaces-w space-w family-width)))))

(define (draw-rounded-rectangle dc x y width heigth)
  (let* ([path     (new dc-path%)]
         [curve-to (λ (x1 y1 x2 y2 x3 y3)
                     (send path curve-to
                           (* width x1) (* heigth y1)
                           (* width x2) (* heigth y2)
                           (* width x3) (* heigth y3)))])
    (send path move-to 0 (/ heigth 10))
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

(define (draw-backgrounds dc coord color)
  (match-define (list x y width heigth) coord)
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (list-ref colors color) 'solid)
  (define rect-path (draw-rounded-rectangle dc x y width heigth))
  (send dc draw-path rect-path))
  
(define (draw-family dc [first-posit 0])
  (let loop ([father-posit first-posit]
             [father-color 4])
    (let ([coord    (hash-ref families-coord father-posit)]
          [color    (get-color father-color)]
          [curr-kin (hash-ref kinship father-posit)])
      (draw-backgrounds dc coord color)
      (when curr-kin 
        (for-each (loop _ color) (kin-children curr-kin))))))

;---

(define (in-the-background? x y curr-posit)
  (match-let ([(list (app (- x _) x*) (app (- y _) y*) width heigth)
               (hash-ref families-coord curr-posit)])
    (and (> x* 0) (> width x*) (> y* 0) (> heigth y*))))

(define (match-family x y)
  (let loop ([father-posit 0])
    (define curr-kin (hash-ref kinship father-posit))
    (if curr-kin
        (let ([matching-child (findf (in-the-background? x y _)
                                     (kin-children curr-kin))])
          (if matching-child
              (loop matching-child)
              father-posit))
        father-posit)))

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

     (define scale-width  0)
     (define scale-heigth 0)

     (define/private (get-x event)
       (/ (send event get-x) scale-width))

     (define/private (get-y event)
       (/ (send event get-y) scale-heigth))
   
     (define/override (on-event evt)
       (define dc (get-dc))
       (cond
         [(send evt button-up?)
          (define family-posit (match-family (get-x evt) (get-y evt)))
          (draw-backgrounds dc (hash-ref families-coord family-posit) 5)
          (sleep/yield 0.5)
          (draw-family dc family-posit)]
         [(send evt button-down?)]
         [(send evt dragging?)]
         [(send evt moving?)]))
     
     (define/public (redraw)
       (define root-width  (hash-ref families-width 0))
       (define root-heigth (hash-ref families-heigth 0))
       (define dc         (get-dc))
       (set! scale-width  (/ width  root-width))
       (set! scale-heigth (/ heigth root-heigth))
       (send dc set-background "LightGray")
       (send dc clear)
       (send dc set-scale scale-width scale-heigth)
       (get-families-coord 0 0 root-width root-heigth)
       (draw-family dc))

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