#lang racket

(provide % ! !! !.!! $! $!! $!.!! use-rewrite show-defs clear-defs
         eval-from-string)

(require "main.rkt" (for-syntax "main.rkt") (for-syntax racket/syntax))

;---

(define-syntax (eval-from-string stx)
  (with-syntax* ([(_ eval-mode str) stx]
                 [expr (string->expr (syntax->datum #'str))])
    #'(eval-mode expr)))

;---