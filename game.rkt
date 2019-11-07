#lang racket/gui

(require fancy-app racket/draw
         (only-in "main.rkt" string->expr α rw-expr rev-rw lst-defs))

;---

(define cv-width  800)
(define cv-heigth 600)
(define colors
  (list
   (make-object color% 225 169 84)
   (make-object color% 178 139 100)
   (make-object color% 98  116 93)
   (make-object color% 36  139 64)
   (make-object color% 1   75  13)
   (make-object color% 166 50  50 3/4)))

;---

(define expression    null)

;---

(define (failed-to-get-expr exn)
  (send message set-label "INVALIDE EXPRESSION!")
  #f)

(define (get-expr)
  (define text (send text-field get-value))
  (send message set-label "Hello")
  (with-handlers ([exn:fail? failed-to-get-expr])
    (set! expression (rev-rw (α (rw-expr (string->expr text)))))))

;---

(define children      (make-hash))
(define arguments     (make-hash))
(define vars          (make-hash))
(define posit-counter 0)

;---

(define (get-affiliation expr)
  (define curr-posit posit-counter)
  (set! posit-counter (add1 curr-posit))
  (define-values (body args)
    (match expr
      [(and (? symbol?)
            (app (hash-ref vars _) (cons lamb-posit arg-posit)))
       (hash-update! arguments lamb-posit
                     (list-update _ arg-posit (cons curr-posit _)))
       (values null null)]
      [`(λ (,args ...) ,body ...)
       (for ([arg       (in-list args)]
             [arg-posit (in-naturals)])
         (hash-set! vars arg (cons curr-posit arg-posit)))
       (values body (map (λ _ null) args))]
      [`(,body ...)
       (values body null)]))
  (hash-set! arguments curr-posit args)
  (define childs (map get-affiliation body))
  (hash-set! children curr-posit childs)
  curr-posit)

(define (get-affiliations)
  (set! children  (make-hash))
  (set! arguments (make-hash))
  (set! vars      (make-hash))
  (set! posit-counter 0)
  (get-affiliation expression)
  (set! vars null))

;---

(define bgs-width  (make-hash))
(define bgs-heigth (make-hash))

;---

(define (get-size curr-posit)
  (define childs (hash-ref children curr-posit))
  (define args   (hash-ref arguments curr-posit))
  (for-each get-size childs)
  (define-values (width heigth) 
    (if (and (null? childs) (null? args))
        (values 100 100)
        (values
         (max 200
              (apply + (map (hash-ref bgs-width _) childs)))
         (+ (* 100 (length args))
            (apply max (map (hash-ref bgs-heigth _) childs))))))
  (hash-set! bgs-width curr-posit (* 11/10 width))
  (hash-set! bgs-heigth curr-posit (* 11/10 heigth)))

(define (get-sizes)
  (set! bgs-width (make-hash))
  (set! bgs-heigth (make-hash))
  (get-size 0))

;---

(define bgs-dimensions (make-hash))
(define lamb-size 0)

;---

(define (get-dimension x y width heigth curr-posit)
  (hash-set! bgs-dimensions curr-posit (list x y width heigth))
  (define childs (hash-ref children curr-posit))
  (define args   (hash-ref arguments curr-posit))
  (unless (null? childs)
    (let* ([childs-len (length childs)]
           [space-wd   (if (= childs-len 1)
                           (/ width 4)
                           (/ (* 1/11 width) (add1 childs-len)))]
           [spaces-hg  (+ y (/ (+ heigth (* lamb-size (length args))) 2))])
      (for/fold ([spaces-wd space-wd])
                ([child     (in-list childs)])
        (define child-wd (hash-ref bgs-width  child))
        (define child-hg (hash-ref bgs-heigth child))
        (get-dimension (+ spaces-wd x) (- spaces-hg (/ child-hg 2))
                       child-wd child-hg child)
        (+ spaces-wd space-wd child-wd)))))

(define (get-dimensions)
  (set! bgs-dimensions (make-hash))
  (define normalizer
    (min (/ cv-width  (hash-ref bgs-width 0))
         (/ cv-heigth (hash-ref bgs-heigth 0))))
  (set! lamb-size (* normalizer 100)) 
  (hash-for-each bgs-width
                 (λ (key val)
                   (hash-set! bgs-width key (* normalizer val))))
  (hash-for-each bgs-heigth
                 (λ (key val)
                   (hash-set! bgs-heigth key (* normalizer val))))
  (define root-width  (hash-ref bgs-width 0))
  (define root-heigth (hash-ref bgs-heigth 0))
  (get-dimension (/ (- cv-width root-width) 2)
                 (/ (- cv-heigth root-heigth) 2)
                 root-width root-heigth 0))

;---

(define (in-the-bg? x y curr-posit)
  (match-let ([(list (app (- x _) x*) (app (- y _) y*) width heigth)
               (hash-ref bgs-dimensions curr-posit)])
    (and (> x* 0) (> width x*) (> y* 0) (> heigth y*))))

(define (match-background x y)
  (let loop ([curr-posit 0])
    (define childs         (hash-ref children curr-posit))
    (define matching-child (findf (in-the-bg? x y _) childs))
    (if matching-child
        (loop matching-child)
        curr-posit)))

;---

(define bgs-color (make-hash))

;---

(define (one-color-available father-color)          
  (define available-colors
    (remove father-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

(define (get-bg-color curr-posit father-color)
  (define color  (one-color-available father-color))
  (define childs (hash-ref children curr-posit))
  (hash-set! bgs-color curr-posit color)
  (for-each (get-bg-color _ color) childs))

(define (get-bgs-color)
  (set! bgs-color (make-hash))
  (get-bg-color 0 5))

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

(define (draw-bg dc color curr-posit)
  (match-define (list x y width heigth)
    (hash-ref bgs-dimensions curr-posit))
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (list-ref colors color) 'solid)
  (define rect-path (draw-rounded-rectangle dc x y width heigth))
  (send dc draw-path rect-path))

(define (draw-bgs dc [curr-posit 0])
  (define color (hash-ref bgs-color curr-posit))
  (draw-bg dc color curr-posit)
  (for-each (draw-bgs dc _) (hash-ref children curr-posit)))

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
       [init-value "(λ x y . x (λ z . z y))"]))

(define button
  (new button% [parent panel]
       [label "Run!"]
       [callback (λ _ (when (get-expr)
                        (send canvas draw)))]))

;---

(define canvas
  (new
   (class canvas%
     (inherit get-dc)

     (define/override (on-event evt)
       (define dc (get-dc))
       (cond
         [(send evt button-up?)
          (define bg-posit (match-background (send evt get-x)
                                             (send evt get-y)))
          (draw-bg dc 5 bg-posit)
          (sleep/yield 0.5)
          (draw-bgs dc bg-posit)]
         [(send evt button-down?)]
         [(send evt dragging?)]
         [(send evt moving?)]))
     
     (define/public (draw)
       (get-affiliations)
       (get-sizes)
       (get-dimensions)
       (get-bgs-color)
       (define dc (get-dc))
       (send dc set-background "LightGray")
       (send dc clear)
       (draw-bgs dc))
      
     (super-new
       [parent         frame]
       [min-width      (+ 20 cv-width)]
       [min-height     (+ 20 cv-heigth)]
       [paint-callback
        (λ (_ dc)
          (send dc set-origin 10 10)
          (get-expr)
          (draw))]))))

;---

(send frame show #t)

;---