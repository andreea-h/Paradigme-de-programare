#lang racket
(define strict (λ (conditie x)
                 (if conditie
                     x ;cand conditie este ceva care nu este #f
                     #f))) ;returneaza #f atunci cand conditie este #f
;(strict #t (/ 5 0)) ;functie stricta->evaluarea parametrilor se face inainte de aplicarea functiei asupra acestora

;;functiile in racket sunt stricte cu exceptia: if, or, and, cond care au evaluarea lenesa, lazy

(if #f (/ 5 0) 'ceva) ;returneaza 'ceva pentru ca conditia este indentic #f

(letrec ((factorial
         (λ (n)
           (if (zero? n)
               1
               (* n (factorial (- n 1)))))))
  (factorial 5))

(define a 10)

(let ((a 1)
      (b (+ a 1))) ;;daca nu era definit a mai sus cu define-> a unbound identifier
  (cons a b))

(let ((a 1))
      (let ((f (λ ()
                 (print a)))
            )
        (let ((a 2))
          (f))))

newline
;even-length? este inchidere functionala->corpul functiei nu este evaluat la momentul definirii ei
(letrec ((even-length? (λ (L)
                         (if (null? L)
                             #t
                             (odd-length? (cdr L)))))
         (odd-length? (λ (L)
                        (if (null? L)
                            #f
                            (even-length? (cdr L))))))
         (even-length? '(1 2 3 4 5 6)))

(define (interval a b step)
  (let iter ((b b) (result null))
    (if (> a b)
        result
        (iter (- b step) (cons b result)))))

(interval 1 30 2)

;;legare dinamica
(define c 1)
(define f (λ (x) (+ x c)))
(f 2)
  