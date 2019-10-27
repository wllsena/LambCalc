#lang racket

(provide % @ ~ $! $!! $!.!! ! !! !.!!)

(require racket/syntax)

;---

(define-match-expander ?equal?
  (syntax-rules ()
    [(?equal? var)
     (? (λ (expr) (equal? var expr)))]))

(define-match-expander ?href
  (syntax-rules ()
    [(?href ht pat)
     (? (λ (key) (hash-has-key? ht key))
        (app (λ (key) (hash-ref ht key)) pat))]))

(define-match-expander ?assoc
  (syntax-rules ()
    [(?assoc lst pat)
     (app (λ (v) (assoc v lst))
          (? and (app cadr pat)))]))

;---

(define (var-replace body from to)
  (let ([to (α to #f)])
    (letrec ([lex-rplc
              (match-lambda 
                [(?equal? from)
                 to]
                [(? symbol? expr)
                 expr]
                [`(,expr1 ,expr2)
                 `(,(lex-rplc expr1) ,(lex-rplc expr2))]
                [`(λ ,arg ,expr)
                 `(λ ,arg ,(lex-rplc expr))])])
      (lex-rplc body))))

(define lz-β
  (match-lambda
    [(or `((λ ,from ,body) ,to)
         `(,(app lz-β `(λ ,from ,body)) ,to))
     (lz-β (var-replace body from to))]
    [expr
     expr]))

(define st-β
  (match-lambda
    [(? symbol? expr)
     expr]
    [`(λ ,arg ,body)
     `(λ ,arg ,(st-β body))]
    [`(,(app st-β expr1) ,(app st-β expr2))
     (match expr1
       [`(λ ,from ,body)
        (st-β (var-replace body from expr2))]
       [_
        `(,expr1 ,expr2)])]))

;---

(define (get-?arg ?bn)
  (match ?bn
    [(box (app add1 ?n))
     (set-box! ?bn ?n)
     (format-symbol "?arg~a" ?n)]
    [#f
     (gensym '?arg)]))

(define (α expr [?bn (box -1)] [hargs (make-immutable-hash)])
  (match expr
    [(or (?href hargs val) (? symbol? val))
     val]
    [`(,expr1 ,expr2)
     `(,(α expr1 ?bn hargs) ,(α expr2 ?bn hargs))]
    [`(λ ,arg ,body)
     (let ([?arg (get-?arg ?bn)])
       `(λ ,?arg ,(α body ?bn (hash-set hargs arg ?arg))))]))

;---

(define rw-expr
  (match-lambda
    [(? symbol? expr)
     expr]

    [`(λ ,(or (? symbol? arg) `(,(? symbol? arg))) ,body ..1)
     `(λ ,arg ,(rw-expr body))]
    [`(λ (,arg ,args ..1) ,body ..1)
     `(λ ,arg ,(rw-expr `(λ ,args ,@body)))]

    [(or `(,expr) `(,expr ()) `(() ,expr) `(λ () ,expr))
     (rw-expr expr)]
    [`(,expr1 ,expr2)
     `(,(rw-expr expr1) ,(rw-expr expr2))]
    [`(,expr1 ,expr2 ,exprs  ..1)
     (rw-expr `((,expr1 ,expr2) ,@exprs))]))

;---

(define lst-defs null)

(define (set-defs! lst)
  (set! lst-defs lst))

(define (expand-defs expr)
  (foldl (λ (val-key expr)
           (match-let ([(list val key) val-key])
             `((λ ,key ,expr) ,val)))
         expr
         lst-defs))

(define rev-rw
  (match-lambda
    [(or (app α (?assoc lst-defs val)) (? symbol? val))
     val]

    [`(λ ,arg ,(app rev-rw `(λ (,args ...) ,body)))
     `(λ (,arg ,@args) ,body)]   
    [`(λ ,arg ,body)
     `(λ (,arg) ,(rev-rw body))]
    
    [`((,expr1 ,expr2) ,expr3)
     `(,(rev-rw expr1) ,(rev-rw expr2) ,(rev-rw expr3))]
    [`(,expr1 ,expr2)
     `(,(rev-rw expr1) ,(rev-rw expr2))]))

;---
 
(define (lazy-eval expr)
  (α (lz-β (α (rw-expr (expand-defs expr)) #f))))

(define (strict-eval expr)
  (α (st-β (α (rw-expr (expand-defs expr)) #f))))

(define (lz.st-eval expr)
  (α (st-β (lazy-eval expr))))

(define (def key val)
  (set! lst-defs (cons (list val key) lst-defs)))

(define (clear-defs)
  (set! lst-defs null))

;---

(begin-for-syntax
  (define-syntax-rule (eval-defs eval-type)
    #'(set-defs! (map (λ (val-key)
                        (match-let ([(list val key) val-key])
                          (list (eval-type val) key)))
                      lst-defs)))
  
  (define-syntax-rule (evaluate stx eval-type)
    (syntax-case stx ()
      [(! expr)
       #'(rev-rw (eval-type (syntax->datum #'expr)))]
      [(! expr no-rw)
       #'(eval-type (syntax->datum #'expr))])))

;---

(define-syntax-rule (% key expr)
  (def (syntax->datum #'key) (syntax->datum #'expr)))

(define-syntax-rule (@)
  lst-defs)

(define-syntax-rule (~)
  (clear-defs))

(define-syntax ($! stx)
  (eval-defs lazy-eval))

(define-syntax ($!! stx)
  (eval-defs strict-eval))

(define-syntax ($!.!! stx)
  (eval-defs lz.st-eval))

(define-syntax (! stx)
  (evaluate stx lazy-eval))

(define-syntax (!! stx)
  (evaluate stx strict-eval))

(define-syntax (!.!! stx)
  (evaluate stx lz.st-eval))