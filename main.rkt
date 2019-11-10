#lang racket

(provide (all-defined-out))

(require (only-in br format-datum)
         fancy-app racket/syntax)

;---

(define (string->expr str)
  (define normalized-str
    (regexp-replaces str
      '([#rx";.*" ""]
        [#rx"lambda" "λ"]
        [#rx"(.*)=(.*)" "% \\1 \\2"]
        [#rx"^([%| ]*)([^(|^λ|^%|^ ]*) ([^#]*)$" "\\1 \\2 (\\3)"]
        [#rx"λ([^.]*)\\." "λ (\\1)"])))
  (format-datum '(~a) normalized-str))

;---

(define-match-expander ?href
  (syntax-rules ()
    [(?href ht pat)
     (? (hash-has-key? ht _)
        (app (hash-ref ht _) pat))]))

(define-match-expander ?assoc
  (syntax-rules ()
    [(?assoc lst pat)
     (app (assoc _ lst)
          (? and (app cadr pat)))]))

;---

(define (var-replace body from to)
  (define to* (α to #f))
  (define lex-rplc
    (match-lambda 
      [(? (equal? from _))
       to*]
      [(? symbol? expr)
       expr]
      [`(,expr1 ,expr2)
       `(,(lex-rplc expr1) ,(lex-rplc expr2))]
      [`(λ ,arg ,expr)
       `(λ ,arg ,(lex-rplc expr))]))
  (lex-rplc body))

;---

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

(define (get-?arg [?bn #f])
  (match ?bn
    [(box (app add1 ?n))
     (set-box! ?bn ?n)
     (format-symbol "?arg~a" ?n)]
    [_
     (gensym '?arg)]))

(define (?arg? sym)
  (regexp-match #rx"^\\?arg.*" (format "~a" sym)))

(define (α expr [?bn (box -1)] [hargs (make-immutable-hash)])
  (match expr
    [`(λ ,arg ,body)
     (define ?arg (get-?arg ?bn))
     `(λ ,?arg ,(α body ?bn (hash-set hargs arg ?arg)))]
    [`(,expr1 ,expr2)
     `(,(α expr1 ?bn hargs) ,(α expr2 ?bn hargs))]
    [(or (?href hargs val) (? ?arg? val))
     val]))

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
  (for/fold ([expr expr])
            ([df   lst-defs])
    (match-define (list val key) df)
    `((λ ,key ,expr) ,val)))

(define rev-rw
  (match-lambda
    [(or (?assoc lst-defs val)
         (? symbol? val)
         (app α (?assoc lst-defs val)))
     val]

    [`(λ ,arg ,(app rev-rw `(λ (,args ...) ,body ...)))
     `(λ (,arg ,@args) ,@body)]
    [`(λ ,arg ,(? symbol? body))
     `(λ (,arg) ,(rev-rw body))]
    [`(λ ,arg ,body)
     `(λ (,arg) ,@(rev-rw body))]

    [`(,(? symbol? expr1) ,expr2)
     `(,(rev-rw expr1) ,(rev-rw expr2))]
    [`((λ ,exprs ...) ,expr2)
     `(,(rev-rw `(λ ,@exprs)) ,(rev-rw expr2))]
    [`(,expr1 ,expr2)
     `(,@(rev-rw expr1) ,(rev-rw expr2))]))

;---

(define (lazy-eval expr)
  (α (lz-β (α (rw-expr (expand-defs expr)) #f))))

(define (strict-eval expr)
  (α (st-β (α (rw-expr (expand-defs expr)) #f))))

(define (lz.st-eval expr)
  (α (st-β (lazy-eval expr))))

;---

(define use-rw #t)

(define (def key val)
  (set-defs! (cons (list val key) lst-defs)))

(define (use-rewrite bool)
  (set! use-rw bool))

(define (show-defs)
  lst-defs)

(define (clear-defs)
  (set-defs! null))

;---

(define-syntax-rule (eval-defs eval-mode)
  (set-defs! (map (match-lambda
                    [(list val key) (list (eval-mode val) key)])
                  lst-defs)))

(define ($!)
  (eval-defs lazy-eval))

(define ($!!)
  (eval-defs strict-eval))

(define ($!.!!)
  (eval-defs lz.st-eval))

;---

(begin-for-syntax
  (define-syntax-rule (evaluate eval-mode)
    (syntax-rules ()
      [(_ . expr)
       (let ([result (eval-mode (syntax->datum #'expr))])
         (match use-rw
           [#t (rev-rw result)]
           [_  result]))])))

(define-syntax-rule (% key expr)
  (def (syntax->datum #'key) (syntax->datum #'expr)))

(define-syntax !
  (evaluate lazy-eval))

(define-syntax !!
  (evaluate strict-eval))

(define-syntax !.!!
  (evaluate lz.st-eval))

;---