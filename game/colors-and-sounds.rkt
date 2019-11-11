#lang racket

(provide stop get-color play-sound reset-colors one-color-available)

(require racket/draw rsound)

;---

(define var-colors
  (list
   (make-color 65  65  65)
   (make-color 130 26  26)
   (make-color 32  56  161)
   (make-color 22  79  15)
   (make-color 97  56  19)
   (make-color 97  28  144)
   (make-color 120 120 120)
   (make-color 97  148 92)
   (make-color 118 131 191)
   (make-color 31  156 156)
   (make-color 191 110 38)
   (make-color 191 178 38)
   (make-color 175 166 140)
   (make-color 191 154 182)))

;---

(define bg-colors
  (list
   (make-color 225 169 84)        ;0
   (make-color 178 139 100)       ;1
   (make-color 98  116 93)        ;2
   (make-color 36  139 64)        ;3
   (make-color 1   75  13)        ;4
   (make-color 169 169 169)       ;5 
   (make-color 75  181 67  3/4)   ;6 green-sucess
   (make-color 192 32  32  3/4))) ;7 red-error

;---
  
(define sounds
  (list
   (rs-read "sounds/error_sound.wav")   ;0 error-sound
   (rs-read "sounds/dinosar_sound.wav") ;1 dinosar_sound)
   (rs-read "sounds/final_sound.wav"))) ;2 final_sound) 

;---

(define var-colors* null)

(define (get-color color-ref)
  (if (number? color-ref)
      (list-ref bg-colors color-ref)
      color-ref))

(define (reset-colors)
  (set! var-colors* (shuffle var-colors)))

(define (one-color-available [parent-color #f])
  (cond
    [parent-color
     (define available-colors (remove parent-color '(0 1 2 3 4)))
     (list-ref available-colors (random 4))]
    [else
     (define color      (car var-colors*))
     (define rst-colors (cdr var-colors*) )
     (if (null? rst-colors)
         (reset-colors)
         (set! var-colors* rst-colors))
     color]))

;---

(define (play-sound sound-ref)
  (define sound (list-ref sounds sound-ref))
  (play sound))

;---