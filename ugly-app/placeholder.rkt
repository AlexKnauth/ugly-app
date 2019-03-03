#lang racket/base

(provide _
         (for-syntax placeholder
                     at-least-one-placeholder
                     arguments/placeholders))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  (define-syntax-class placeholder
    [pattern {~literal _}])

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
