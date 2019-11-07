#lang racket/gui

(require fancy-app racket/draw
         (only-in "main.rkt" string->expr α rw-expr rev-rw lst-defs))

;---

(define cv-width  1000)
(define cv-heigth 1000)
(define colors
  (list
   (make-object color% 225 169 84)
   (make-object color% 178 139 100)
   (make-object color% 98  116 93)
   (make-object color% 36  139 64)
   (make-object color% 1   75  13)
   (make-object color% 75  181 67 3/4)
   (make-object color% 192 32  32 3/4)))

;---

(define expression    null)

;---

(define (failed-to-get-expr exn)
  (send message set-label "INVALIDE EXPRESSION!")
  #f)

(define (rw.α expr)
  (rev-rw (α (rw-expr expr) #f)))

(define (get-expr)
  (define text (send text-field get-value))
  (send message set-label "Hello")
  (with-handlers ([exn:fail? failed-to-get-expr])
    (set! expression (rw.α (string->expr text)))))

;---

(define expressions   (make-hash))
(define children      (make-hash))
(define parents       (make-hash))
(define arguments     (make-hash))
(define vars          (make-hash))
(define posit-counter 0)

;---

(define (get-affiliation expr)
  (define curr-posit posit-counter)
  (set! posit-counter (add1 curr-posit))
  (hash-set! expressions curr-posit expr)
  
  (define-values (body args)
    (match expr
      [(and (or (? symbol?) `(? symbol?))
            (app (hash-ref vars _) (cons lamb-posit arg-posit)))
       (hash-update! arguments lamb-posit
                     (list-update _ arg-posit
                                  (append _ (list curr-posit))))
       (values null null)]
      [`(λ (,args ...) ,body ...)
       (for ([arg       (in-list args)]
             [arg-posit (in-naturals)])
         (hash-set! vars arg (cons curr-posit arg-posit)))
       (values body (map (λ _ null) args))]
      [`(,body ...)
       (values body '(()))]))
  
  (hash-set! arguments curr-posit args)
  (define childs (map get-affiliation body))
  (for-each (hash-set! parents _ curr-posit) childs)
  (hash-set! children curr-posit childs)
  curr-posit)

(define (get-affiliations )
  (hash-clear! expressions)
  (hash-clear! children   )
  (hash-clear! parents    )
  (hash-clear! arguments  )
  (hash-clear! vars       )
  (set! posit-counter 0)
  (get-affiliation expression)
  (hash-clear! vars       ))
  

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
  (hash-clear! bgs-width )
  (hash-clear! bgs-heigth)
  (get-size 0))

;---

(define bgs-dimensions (make-hash))
(define lamb-size      0)

;---

(define (get-dimension x y width heigth curr-posit)
  (hash-set! bgs-dimensions curr-posit (list x y width heigth))
  (define childs (hash-ref children curr-posit))
  (define args   (hash-ref arguments curr-posit))
  (unless (null? childs)
    (let* ([childs-len (length childs)]
           [space-wd   (/ (* 1/11 width) (add1 childs-len))]
           [spaces-hg  (+ y (/ (+ heigth (* lamb-size (length args))) 2))])
      (cond
        [(= childs-len 1)
         (define child    (car childs))
         (define child-wd (hash-ref bgs-width  child))
         (define child-hg (hash-ref bgs-heigth child))
         (get-dimension (+ x (/ (- width child-wd) 2))
                        (- spaces-hg (/ child-hg 2))
                        child-wd child-hg child)]
        [else
         (for/fold ([spaces-wd space-wd])
                   ([child     (in-list childs)])
           (define child-wd (hash-ref bgs-width  child))
           (define child-hg (hash-ref bgs-heigth child))
           (get-dimension (+ spaces-wd x) (- spaces-hg (/ child-hg 2))
                          child-wd child-hg child)
           (+ spaces-wd space-wd child-wd))]))))

(define (get-dimensions)
  (hash-clear! bgs-dimensions)
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
  (match-let* ([(list (app (- x _) x*) (app (- y _) y*) width heigth)
                (hash-ref bgs-dimensions curr-posit)]
               [(cons var-x var-y) (cons (/ width 50) (/ heigth 50))])
    (and (> x* var-x) (> (- width var-x) x*)
         (> y* var-y) (> (- heigth var-y) y*))))

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

(define (one-color-available parent-color)          
  (define available-colors
    (remove parent-color '(0 1 2 3 4)))
  (list-ref available-colors (random 4)))

(define (get-bg-color curr-posit parent-color)
  (define color  (one-color-available parent-color))
  (define childs (hash-ref children curr-posit))
  (hash-set! bgs-color curr-posit color)
  (for-each (get-bg-color _ color) childs))

(define (get-bgs-color)
  (hash-clear! bgs-color)
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

(define (draw-bg dc curr-posit color)
  (match-define (list x y width heigth)
    (hash-ref bgs-dimensions curr-posit))
  (send dc set-pen "DarkGray" 1 'hilite)
  (send dc set-brush (list-ref colors color) 'solid)
  (define rect-path (draw-rounded-rectangle dc x y width heigth))
  (send dc draw-path rect-path))

(define (draw-bgs dc [curr-posit 0])
  (define color (hash-ref bgs-color curr-posit))
  (draw-bg dc curr-posit color)
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
       [init-value "(λ x . x) (λ x . x)"]))

(define button
  (new button% [parent panel]
       [label "Run!"]
       [callback (λ _ (when (get-expr)
                        (send canvas draw)))]))

;---

(define (display-answer dc bg-posit color)
  (draw-bg dc bg-posit color)
  (sleep 1/3)
  (draw-bgs dc bg-posit))

(define (remove-someone posit)
  (hash-remove! expressions posit)
  (hash-remove! children    posit)
  (hash-remove! parents     posit)
  (hash-remove! arguments   posit)
  (hash-remove! bgs-color   posit))

(define (remove-family posit)
  (define childs (hash-ref children posit))
  (remove-someone posit)
  (for-each remove-family childs))

(define (inherit-posit parent curr-posit)
  (define grandparent (hash-ref parents parent))
  (define childs      (hash-ref children parent))
  (hash-update! children grandparent 
                (λ (lst)
                  (list-set lst (index-of lst parent) curr-posit)))
  (hash-set! parents curr-posit grandparent)
  (remove-family parent))

(define (updates-var expr var)
  (define curr-posit  (get-affiliation expr))
  (define grandparent (hash-ref parents var))
  (inherit-posit var curr-posit)
  (values curr-posit grandparent))

(define (reduce lamb body parent siblings args)
  (define vars        (car args))
  (define body-expr   (rw.α (hash-ref expressions body)))
  (hash-update! arguments lamb cdr)
  (hash-update! children parent (remove body _))
  (remove-family body)
  (for ([var (in-list vars)])
    (define-values (curr-posit parent-posit)
      (updates-var body-expr var))
    (define parent-color (hash-ref bgs-color parent-posit))
    (get-bg-color curr-posit parent-color)))
        
(define (try-to-reduce dc lamb body)
  (cond
    [(and (> lamb 0) (> body 0))
     (define parent   (hash-ref parents lamb))
     (define siblings (hash-ref children parent))
     (define args     (hash-ref arguments lamb))
     (cond
       [(and parent
             (> (length siblings) 1)
             (> (length args) 0)
             (= (car siblings) lamb)
             (= (cadr siblings) body))
        (display-answer dc body 5)
        (reduce lamb body parent siblings args)]
       [(display-answer dc body 6)])]
    [(display-answer dc body 6)]))

;---

(define canvas
  (new
   (class canvas%
     (inherit get-dc)

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
          (set! bg-selected bg-posit)]))

     (define/public (redraw)
       (get-sizes)
       (get-dimensions)
       (define dc (get-dc))
       (send dc set-background "DarkGray")
       (send dc clear)
       (draw-bgs dc))
     
     (define/public (draw)
       (get-affiliations)
       (get-bgs-color)
       (redraw))
      
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