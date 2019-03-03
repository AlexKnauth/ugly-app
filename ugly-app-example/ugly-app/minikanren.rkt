#lang racket/base

(provide define-relation
         (all-from-out
          ugly-app
          icfp2017-minikanren/mk/mk))

(require syntax/parse/define
         ugly-app
         icfp2017-minikanren/mk/mk)

(define-simple-macro
  (define-relation name:id ...
    [usage-pattern:expr body:expr ...]
    ...)
  (define-ugly-function name ...
    [usage-pattern body ...]
    ...))
