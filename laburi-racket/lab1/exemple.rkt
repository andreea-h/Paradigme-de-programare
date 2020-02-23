#lang racket


(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;(factorial 2)
;(factorial 5)
;(factorial -1)

(define (sumList L)
  (if (null? L)
      0
      (+ (car L) (sumList (cdr L)))))

;(sumList '(1 2 3 4))
;(sumList '(1 2))

(define (rev L)
  (if (null? L)
      '()
       (append (rev (cdr L)) (list(car L)))))


(define (palindrome? L)
  (if (equal? (rev L) L)
      #t
      #f))


(define (num->base n b)
  (if (= n 0)
      null
    (append (num->base (quotient n b) b) (list (modulo n b)))))

(define (all-palindromes? n Bases)
  (unless (empty? Bases)
    (and (palindrome? (num->base n (first Bases))))
    (and (all-palindromes? n (rest Bases)))))

(all-palindromes? 585 '(2 10))

(define (palindromes-to-n n Bases)
  (if (>= n 0)
      (if (all-palindromes? n Bases)
         ; (list n)
          (append (palindromes-to-n (- n 1) Bases) (list n))
          (palindromes-to-n (- n 1) Bases))
          ;null)
       null))

;(palindromes-to-n 100 '(2 10))

(define (iterate lst)
  (unless (empty? lst)
    (displayln (first lst))
    (iterate (rest lst))))

;(iterate '(1 2 3 4))

