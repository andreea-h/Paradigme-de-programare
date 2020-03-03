#lang racket
(define append2 (λ (l1 l2) (append l2 l1)))
(append2 '(1 2 3) '(4 5 6))

(define (append3 l1 l2)
  (append l2 l1))

(append3 '(1 2 3) '(4 5 6))