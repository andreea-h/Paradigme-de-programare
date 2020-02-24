#lang racket
(define (member x L)
  (if (null? L)
      #f
      (if (equal? x (car L))
          (cdr L)
          (member x (cdr L)))))

(member 2 '(1 7 3 4))