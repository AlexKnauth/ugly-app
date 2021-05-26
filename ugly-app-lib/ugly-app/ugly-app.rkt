#lang racket/base

(provide #%app _
         (for-syntax mouse-over-tooltips
                     partial-application-tooltips))

(require (only-in racket/base [#%app app])
         "placeholder.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "ugly-name.rkt"))

(define-syntax #%app
  (lambda (stx)
    (syntax-parse stx
      #:track-literals
      [(_ {~alt {~once {~var u (static ugly-name-piece? "ugly name piece")}}
                {~var o (static ugly-name-piece? "ugly name piece")}
                {~not {~var _ (static ugly-name-piece? "ugly name piece")}}}
          ...)
       #:cut
       #:do [(define s (syntax-e (attribute u)))
             (define f (attribute u.value))]
       #:fail-when (check-ugly-name-compatibility (attribute u) (attribute o))
       (format "unexpected ugly-name-piece, incompatible with ~a" s)
       (f (datum->syntax stx (stx-cdr stx) stx stx))]
      [{~and :at-least-one-placeholder
             ~!
             (_ . args:arguments/placeholders)}
       (syntax-property
        #'(λ (args.parameter ...)
            (app args.argument ...))
        mouse-over-tooltips
        (partial-application-tooltips
         stx
         (attribute args.placeholder)))
       ]
      [(_ f:expr a ...)
       #'(app f a ...)])))


(begin-for-syntax
  (define mouse-over-tooltips 'mouse-over-tooltips)

  ;; Syntax String -> [Maybe MouseOverTooltipVector]
  (define (stx-tooltip stx msg)
    (define pos (syntax-position stx))
    (define span (syntax-span stx))
    (and pos span
         (vector stx pos (+ pos span) msg)))
  ;; Syntax String -> [Maybe MouseOverTooltipVector]
  (define (stx-tooltip/sub1 stx msg)
    (define pos (syntax-position stx))
    (define span (syntax-span stx))
    (and pos span
         (vector stx (sub1 pos) (sub1 (+ pos span)) msg)))

  ;; Syntax [Listof Syntax] -> [Listof MouseOverTooltipVector]
  (define (partial-application-tooltips stx placeholders)
    (define fn-tooltip
      (stx-tooltip
       stx
       "this application is automatically a function using _"))
    (define plc-tooltips
      (for/list ([placeholder (in-list placeholders)])
        (stx-tooltip/sub1
         placeholder
         "_ is the parameter to the anonymous function")))
    (filter vector? (cons fn-tooltip plc-tooltips))))
