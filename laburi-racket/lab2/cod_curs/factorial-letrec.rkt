#lang racket
(define factorial (λ (n)
                    (letrec ((factorial-aux (λ (nr rezultat)
                                              (if (= nr 1)
                                                  rezultat
                                                  (factorial-aux (sub1 nr) (* nr rezultat))
                                                  )
                                              )))
                      (factorial-aux n 1)
                      
                      )))
(factorial 4)

(define evens (λ (L)
                (if (null? L)
                    '()
                    ( (lambda (evenscdr) ; folosesc lambda pentru a lega la un nume
                        (if (even? (car L))
                            (cons (car L) evenscdr)
                            evenscdr)
                        )
                      (evens (cdr L)) ; folosesc codul duplicat anterior ca parametru actual
                      ))))

(define evens1 (λ (L)
                (if (null? L)
                    '()
                    ( (λ (evenscdr)
                        (if (even? (car L))
                            (cons (car L)
                                  evenscdr)
                            evenscdr))
                     (evens (cdr L))))))
                                   
(evens1 (list 1 2 3 4 5))

(define (evensElms L result)
  (if (null? L)
      result
      (if (even? (car L))
          (evensElms (cdr L) (append result (list (car L))))
          (evensElms (cdr L) result))))

(evensElms '(1 2 3 4 5 6 7 8) null)