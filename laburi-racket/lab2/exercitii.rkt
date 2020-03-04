#lang racket
(require (lib "trace.ss"))

;;recursivitate pe coada
(define factorial
  (λ (n)
    (letrec ((aux (λ (n rezP)
                    (if (= n 0)
                        rezP
                        (aux (sub1 n) (* n rezP))))))
      (aux n 1))))

(trace factorial)
(factorial 4)


;;recursivitate pe coada
(define (fact-iter counter result)
  (if (= counter 0)
      result
      (fact-iter (- counter 1) (* result counter))))
(fact-iter 5 1)