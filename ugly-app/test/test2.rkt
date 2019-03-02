#lang racket/base

(require racket/match
         ugly-app)
(module+ test
  (require rackunit))

;; -------------------------------------------------------------------

(define-ugly-function = [(a = b) (equal? a b)])

(define ∅ (hash))
(define-ugly-function / → [(ρ / x → v) (hash-set ρ x v)])
(define-ugly-function ! [(ρ ! x) (hash-ref ρ x)])

(define-ugly-function ⊢ ⇓ ⇒ ⇐
  [(ρ ⊢ e ⇓)
   (match e
     [(? number? v) v]
     [(? symbol? x) (ρ ! x)]
     [`(ann ,e : ,_) (ρ ⊢ e ⇓)]
     [`(λ (,x) ,b) (λ (a) ((ρ / x → a) ⊢ b ⇓))]
     [`(,f ,a) ((ρ ⊢ f ⇓) (ρ ⊢ a ⇓))])]
  [(Γ ⊢ e ⇒)
   (match e
     [(? number? v) 'Num]
     [(? symbol? x) (Γ ! x)]
     [`(ann ,e : ,τ) (and (Γ ⊢ e ⇐ τ) τ)]
     [`(λ (,x) ,b) (error "cannot infer parameter type")]
     [`(,f ,a)
      (match (Γ ⊢ f ⇒)
        [`(,A → ,B) (and (Γ ⊢ a ⇐ A) B)]
        [F (error 'Γ⊢e⇒ "expected a function, given: ~a in ~a" F f)])])]
  [(Γ ⊢ e ⇐ τ)
   (match e
     [`(ann ,e : ,τ2) (and (τ = τ2) (Γ ⊢ e ⇐ τ2))]
     [`(λ (,x) ,b)
      (match τ
        [`(,A → ,B) ((Γ / x → A) ⊢ b ⇐ B)]
        [_ (error "cannot infer parameter type")])]
     [_
      ((Γ ⊢ e ⇒) = τ)])])

;; ---------------------------------------------

(module+ test
  (check-equal? (∅ ⊢ `(((λ (f) (λ (a) (f a)))
                        (λ (x) x))
                       2)
                   ⇓)
                2)

  (check-equal? (∅ ⊢ `(((ann (λ (f) (λ (a) (f a)))
                             : ((Num → Num) → (Num → Num)))
                        (λ (x) x))
                       2)
                   ⇒)
                'Num)
  )
