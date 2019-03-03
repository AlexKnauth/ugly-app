#lang racket/base

(provide _
         (for-syntax placeholder
                     at-least-one-placeholder
                     arguments/placeholders))

(require (only-in racket/base [_ __])
         syntax/parse
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx))

;; The `_` identifier provided by this module has 3 meanings:
;;  - placeholder for partial function application
;;  - wildcard in match patterns
;;  - wildcard in syntax-parse patterns
;; However, it doesn't have one meaning that `_` from racket originally had:
;;  - It cannot be used as a wildcard in syntax-case / syntax-rules patterns,
;;    so use syntax-parse instead of syntax-case. It's better anyway.

(begin-for-syntax
  (struct normal+pattern [normal pattern]
    #:property prop:procedure (struct-field-index normal)
    #:property prop:pattern-expander
    (λ (self) (normal+pattern-pattern self))))

(define-syntax _
  (normal+pattern
   (lambda (stx)
     (cond [(identifier? stx)  (syntax/loc stx __)]
           [(stx-pair? stx)    (datum->syntax stx (cons '#%app stx) stx stx)]
           [else
            (raise-syntax-error #f "used out of context" stx)]))
   (lambda (stx)
     (cond [(identifier? stx)  (syntax/loc stx __)]
           [(and (stx-pair? stx) (identifier? (stx-car stx)))
            (datum->syntax stx
                           (cons (syntax/loc (stx-car stx) __) (stx-cdr stx))
                           stx
                           stx)]
           [else
            (raise-syntax-error #f "used out of context" stx)]))))

;; -------------------------------------------------------------------

(begin-for-syntax
  (define-syntax-class placeholder
    [pattern {~or {~literal _} {~literal __}}])

  (define-syntax-class argument/placeholder
    [pattern blank:placeholder
      #:with tmp (generate-temporary #'blank)
      #:with [parameter ...] #'[tmp]
      #:with argument #'tmp]
    [pattern argument
      #:with [parameter ...] '()])

  (define-syntax-class at-least-one-placeholder
    [pattern (_ ... :placeholder _ ...)])

  (define-syntax-class arguments/placeholders
    [pattern (arg:argument/placeholder ...)
      #:with [parameter ...] #'[arg.parameter ... ...]
      #:with [argument ...] #'[arg.argument ...]])
  )
