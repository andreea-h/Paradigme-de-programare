#lang racket
((λ L L) '(1 2 3 4))
(define f (λ (x) (λ (y) (+ x y))))
(f 1)
((f 1) 2) ;;f este o functie curry

(define a 1)
(define (my_add x)
  (+ x a))
(my_add 3)
;(define a 3) a nu poate fi redefinit
(my_add 3)
