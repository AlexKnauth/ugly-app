#lang racket/base

(provide #%module-begin #%datum #%top #%top-interaction
         require
         module+
         #%app _
         function -> match with fun
         = def and data \| let rec in
         if then else
         < <= + - * / ^
         )

(require syntax/parse/define
         ugly-app
         (prefix-in rkt: (combine-in racket/base racket/match))
         (for-syntax racket/base))

;; -------------------------------------------------------------------

(define-ugly-macro function -> match with fun
  [(function {~seq p -> b} ...)
   (λ (x) (match x with {~@ p -> b} ...))]
  [(match e with {~seq p -> b} ...)
   (rkt:match e [p b] ...)]
  [(fun -> b) b]
  [(fun p0 p ... -> b) (function p0 -> (fun p ... -> b))])

(define-ugly-macro = def and data \| let rec in
  [(a = b) (equal? a b)]
  [(def x0 a0 ... = b0 {~seq and x a ... = b} ...)
   (begin
     (define x0 (fun a0 ... -> b0))
     (define x (fun a ... -> b))
     ...)]
  [(data t x ... = v0 f0 ... {~seq \| v f ...} ...)
   #:with [f0* ...] (generate-temporaries (attribute f0))
   #:with [[f* ...] ...] (map generate-temporaries (attribute f))
   (begin
     (struct v0 [f0* ...] #:transparent)
     (struct v [f* ...] #:transparent)
     ...)]
  [(let x0 a0 ... = b0 {~seq and x a ... = b} ... in body)
   (rkt:let ([x0 (fun a0 ... -> b0)]
             [x (fun a ... -> b)]
             ...)
     body)]
  [(let rec x0 a0 ... = b0 {~seq and x a ... = b} ... in body)
   (rkt:letrec ([x0 (fun a0 ... -> b0)]
                [x (fun a ... -> b)]
                ...)
     body)])

(define-ugly-macro if then else
  [(if a then b {~seq else if c then d} ... else e)
   (cond [a b] [c d] ... [rkt:else e])])

;; ---------------------------------------------------------

(define-ugly-function < [(a < b) (rkt:< a b)])
(define-ugly-function <= [(a <= b) (rkt:<= a b)])
(define-ugly-function + [(a {~seq + b} ...) (apply rkt:+ a b)])
(define-ugly-function - [(a {~seq - b} ...) (apply rkt:- a b)]
  [(- b) (rkt:- b)])
(define-ugly-function * [(a {~seq * b} ...) (apply rkt:* a b)])
(define-ugly-function / [(a {~seq / b} ...) (apply rkt:/ a b)])
(define-ugly-function ^ [(a ^ b) (expt a b)])


