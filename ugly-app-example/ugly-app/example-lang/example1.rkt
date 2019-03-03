#lang s-exp ugly-app/example-lang

(data listof x = Empty
              \| Cons x (listof x))

(def length lst =
  (match lst with
    (Empty)      -> 0
    (Cons _ rst) -> (1 + (length rst))))

(def append as bs =
  (match as with
    (Empty)     -> bs
    (Cons a as) -> (Cons a (append as bs))))

(def reverse lst =
  (match lst with
    (Empty)      -> (Empty)
    (Cons x rst) -> (append (reverse rst) (Cons x (Empty)))))

