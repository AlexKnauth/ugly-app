#lang racket/base

(require (only-in racket/base
                  [= =?]
                  [< <?]
                  [<= <=?]
                  [+ add]
                  [- subtract]
                  [- negate]
                  [* multiply]
                  [/ divide]
                  [if ite]
                  [else cond-else])
         ugly-app)
(module+ test
  (require rackunit))

;; -------------------------------------------------------------------

(module+ test
  (define-ugly-function = [(a = b) (=? a b)])
  (define-ugly-function < [(a < b) (<? a b)])
  (define-ugly-function <= [(a <= b) (<=? a b)])
  (define-ugly-function + [(a {~seq + b} ...) (apply add a b)])
  (define-ugly-function - [(a - b) (subtract a b)] [(- b) (negate b)])
  (define-ugly-function * [(a * b) (multiply a b)])
  (define-ugly-function / [(a / b) (divide a b)])
  (define-ugly-function ^ [(a ^ b) (expt a b)])

  (define-ugly-macro if then else
    [(if a then b {~seq else if c then d} ... else e)
     (cond [a b] [c d] ... [cond-else e])])

  ;; ---------------------------------------------

  (check-equal? (1 + 2) 3)
  (check-equal? (1 + 2 + 3 + 4 + 5) 15)
  (check-equal? (map (1 + _) (list 1 2 3 4)) (list 2 3 4 5))
  (check-equal? (map (1 + _ + 3) (list 1 2 3 4)) (list 5 6 7 8))
  (check-equal? (map (_ - 1) (list 1 2 3 4)) (list 0 1 2 3))
  (check-equal? (map (2 * _) (list 1 2 3 4)) (list 2 4 6 8))
  (check-equal? (map (2 ^ _) (list 1 2 3 4)) (list 2 4 8 16))
  (check-equal? (map (_ ^ 2) (list 1 2 3 4)) (list 1 4 9 16))
  (check-equal? (map (- _) (list 1 2 3 4)) (list -1 -2 -3 -4))

  (define (fact n)
    (if (n = 0)
     then 1
     else (n * (fact (n - 1)))))

  (check-equal? (fact 5) 120)

  (define (fib n)
    (if (n = 0)
     then 0
     else if (n = 1)
     then 1
     else ((fib (n - 2)) + (fib (n - 1)))))

  (check-equal? (fib 5) 5)
  (check-equal? (fib 7) 13)
  (check-equal? (fib 10) 55)
  (check-equal? (fib 12) 144)
  (check-equal? (fib 16) 987)
  )
