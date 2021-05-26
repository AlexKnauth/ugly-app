#lang info

(define collection 'multi)

(define deps
  '("base"
    "ugly-app-lib"
    "ugly-app-example"
    "ugly-app-test"
    ))

(define implies
  '("ugly-app-lib"
    "ugly-app-example"
    "ugly-app-test"
    ))

(define update-implies
  '("ugly-app-lib"
    "ugly-app-example"
    "ugly-app-test"
    ))
