#lang racket

(provide get-color play-sound reset-colors one-color-available)

(require racket/draw rsound)

;---

(define var-colors
  (list
   (make-color 87  87  87)
   (make-color 173 35  35)
   (make-color 42  75  215)
   (make-color 29  105 20)
   (make-color 129 74  25)
   (make-color 129 38  192)
   (make-color 160 160 160)
   (make-color 129 197 122)
   (make-color 157 175 255)
   (make-color 41  208 208)
   (make-color 255 146 51)
   (make-color 255 238 51)
   (make-color 233 222 187)
   (make-color 255 205 243)))

;---

(define bg-colors
  (list
   (make-color 225 169 84)        ;0
   (make-color 178 139 100)       ;1
   (make-color 98  116 93)        ;2
   (make-color 36  139 64)        ;3
   (make-color 1   75  13)        ;4
   "DarkGray"                     ;5 
   (make-color 75  181 67  3/4)   ;6 green-sucess
   (make-color 192 32  32  3/4))) ;7 red-error

;---
  
(define sounds
  (list
   (rs-read "sounds/error_sound.wav")   ;0 error-sound
   (rs-read "sounds/dinosar_sound.wav") ;1 dinosar_sound)
   (rs-read "sounds/final_sound.wav")))  ;2 final_sound) 

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