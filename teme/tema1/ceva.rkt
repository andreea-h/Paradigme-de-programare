#lang racket
(define x 10)
(let* ((x 1)
       (y (add1 x))
       (z (λ () y))
       (t (z))
       (L (list x y z t)))
  (filter number? L))

(define (example1 a b)
  (let [(sum (λ (a b) (+ (foldr + 0 a)
                         (foldl + 0 b)))) (a b) (b a)]
    (sum a b)))