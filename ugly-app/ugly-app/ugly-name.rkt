#lang racket/base

(provide id-join
         ugly-name-piece
         ugly-name-piece?
         check-ugly-name-compatibility)

(require racket/string
         syntax/stx)

;; An "ugly" name is made up pieces. For example, the ugly name "if then else"
;; is made up of the pieces `if`, `then`, and `else`.

;; Id -> String
(define (id->str id) (symbol->string (syntax-e id)))
;; Syntax String -> Id
(define (str->id ctx s) (datum->syntax ctx (string->symbol s) ctx ctx))

;; [Listof Id] Id -> Id
(define (id-join ids sep)
  (define ctx (if (stx-null? ids) sep (stx-car ids)))
  (define str-sep (id->str sep))
  (define str-ids (stx-map id->str ids))
  (str->id ctx (string-join str-ids str-sep)))

(struct ugly-name-piece [parser]
  #:property prop:procedure (struct-field-index parser))

;; Id [Listof Id] -> [Maybe Id]
(define (check-ugly-name-compatibility x1 ys)
  (define xv (syntax-local-value x1))
  (for/or ([y (in-list ys)])
    (define yv (syntax-local-value y))
    (and (not (equal? xv yv)) y)))
