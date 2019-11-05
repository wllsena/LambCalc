#lang racket

(provide % ! !! !.!! $! $!! $!.!! use-rewrite show-defs clear-defs
         read-syntax  #%module-begin #%top-interaction)

(require "main.rkt" fancy-app)

;---

(define (read-syntax path port)
  #`(module main "lambcalc.rkt"
      #,@(let* ([lines (port->lines port)]
                [exprs (map (string->expr _) lines)])
           (remove* '(()) exprs))))

;---