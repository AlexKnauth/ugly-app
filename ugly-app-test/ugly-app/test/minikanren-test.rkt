#lang racket/base

(require ugly-app/minikanren)
(module+ test
  (require rackunit))

(define-relation = [(a = b) (== a b)])
(define-relation ≠ [(a ≠ b) (=/= a b)])

(define ∅ '())
(define-ugly-function ! / →
  [(ρ / x → v) (cons (list x v) ρ)]
  [(ρ ! x → xv)
   (fresh (y yv rst)
     (ρ = (cons (list y yv) rst))
     (conde [(x = y) (xv = yv)]
            [(x ≠ y) (rst ! x → xv)]))])

;; e = n
;;   | x
;;   | `(ann ,e : ,τ)
;;   | `(λ (,x) ,e)
;;   | `(app ,e ,e)
;; τ = 'Num
;;   | `(,τ → ,τ)
;; v = n
;;   | `(closure ,ρ (,x) ,e)
;; ρ = `((,x ,v) ...)

(define-relation ⊢ ⇓ :
  [(ρ ⊢ e ⇓ v)
   (conde
    [(numbero e) (e = v)]
    [(symbolo e) (ρ ! e → v)]
    [(fresh (e1 τ)
       (e = `(ann ,e1 : ,τ))
       (ρ ⊢ e1 ⇓ v))]
    [(fresh (x b)
       (e = `(λ (,x) ,b))
       (v = `(closure ,ρ (,x) ,b)))]
    [(fresh (f a fρ fx fb av)
       (e = `(app ,f ,a))
       (ρ ⊢ f ⇓ `(closure ,fρ (,fx) ,fb))
       (ρ ⊢ a ⇓ av)
       ((fρ / fx → av) ⊢ fb ⇓ v))])]
  [(Γ ⊢ e : τ)
   (conde
    [(numbero e) (τ = 'Num)]
    [(symbolo e) (Γ ! e → τ)]
    [(fresh (e1)
       (e = `(ann ,e1 : ,τ))
       (Γ ⊢ e1 : τ))]
    [(fresh (x b A B)
       (e = `(λ (,x) ,b))
       (τ = `(,A → ,B))
       ((Γ / x → A) ⊢ b : B))]
    [(fresh (f a A B)
       (e = `(app ,f ,a))
       (Γ ⊢ f : `(,A → ,τ))
       (Γ ⊢ a : A))])])

;; ---------------------------------------------

(module+ test
  (check-equal? (run 1 (v) (∅ ⊢ 5 ⇓ v))
                '((5)))
  (check-equal? (run 1 (v)
                  (∅ ⊢ `(app (app (λ (f) (λ (a) (app f a)))
                                  (λ (x) x))
                         2)
                     ⇓ v))
                '((2)))

  (check-equal? (run 1 (τ)
                  (∅ ⊢ `(app (app (ann (λ (f) (λ (a) (app f a)))
                                       : ((Num → Num) → (Num → Num)))
                                  (λ (x) x))
                             2)
                     : τ))
                '((Num)))

  (check-equal? (run 1 (τ)
                  (∅ ⊢ `(app (app (λ (f) (λ (a) (app f a)))
                                  (λ (x) x))
                             2)
                     : τ))
                '((Num)))
  )
