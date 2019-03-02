#lang racket/base

(provide #%app)

(require (only-in fancy-app [#%app fancy-app])
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
      [(_ f:expr a ...)
       #'(fancy-app f a ...)])))
