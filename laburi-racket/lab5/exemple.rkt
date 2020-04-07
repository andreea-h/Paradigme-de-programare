#lang racket
(foldl string-append " " '("A" "n" "a" " ") '("a" "r" "e" "") '("m" "e" "r" "e"))

;(map add1 '(0 (1 2)))
   ;  (apply list 1 '(2 3 ))
(define (f L acc)
  (foldl (λ (x y) (cons acc y)) '() L))

(f '(1 2 3) '(4 5 6))

(map (λ (x) (x 3 2))
     (list + - *))