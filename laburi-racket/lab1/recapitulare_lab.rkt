#lang racket
(define (num->base n b)
  (if (= n 0)
      null
      (append (num->base (quotient n b) b) (list (modulo n b)))))

(num->base 489 2)

(define (rev L)
  (if (null? L)
      null
      (append (rev (cdr L)) (list (car L)))))

;recursivitate pe stiva
(define rev1 (λ (L)
               (if (null? L)
                   null
                   (append (rev1 (cdr L)) (list (car L))))))

;recursivitate pe coada
(define (rev-iter L result)
  (if (null? L)
      result
      (rev-iter (cdr L) (append (list (car L)) result))))

(define (rev2 L)
  (rev-iter L null))

(define (divizible_by_3 n)
  (if (= 0 (modulo n 3))
      #t
      #f))

;functie care verifica daca toate elementele dintr-o lista sunt divizibile cu 3
(define (verify L)
  (if (> (length L) 1)
      (and (verify (cdr L)) (divizible_by_3 (car L)))
      (divizible_by_3 (car L))))


;;recursivitate pe coada
(define (remove-duplicates-iter L result)
  (if (null? L)
      result
      (if (equal? (member (car L) result) #f)
          (remove-duplicates-iter (cdr L) (append result (list (car L))))
          (remove-duplicates-iter (cdr L) result))))
      
(define (remove-duplicates L)
  (remove-duplicates-iter L null))

;;recursivitate pe stiva
(define (remove-duplicates-right L)
  (if (null? L)
      null
      (if (equal? (member (car L) (cdr L)) #f) ;element cu aparitie unica
          (append (list (car L)) (remove-duplicates-right (cdr L)))
          (remove-duplicates-right (cdr L)))))

(define (common-suffix-iter L1 L2 n1 n2 result)
  (if (or (<= n1 1) (<= n2 1))
      result
      (if (equal? (drop L1 n1) (drop L2 n2))
          (common-suffix-iter L1 L2 (- n1 1) (- n2 1) (drop L1 n1))
          result)))

(define (common-suffix L1 L2)
  (common-suffix-iter L1 L2 (sub1 (length L1)) (sub1 (length L2)) null))

(define (sublists L)
  (if (null? L)
      null
      (let ((rest (sublists (cdr L))))
        (append rest ((λ (x)
                          (cons (car L) x))
                        rest)))))


(rev '(1 2 3 4 5 6))
(rev1 '(1 2 3 4 5 6))
(rev2 '(1 2 3 4 5 6))
(verify '(3 6 8 9 12))
(verify '(3 6 9 12 6 36))
(remove-duplicates '(1 2 3 3 4 5 6 3 3))
(remove-duplicates-right '(1 2 3 3 4 5 6 3 3))
(common-suffix '(1 2 3 4 5 6 7) '(1 2 3 9 10 12 5 6 7))
(sublists '(1 2 3))

(define sum '(+ 2 3))
(car (cdr (cdr sum)))
(car (cdr sum))
(cdr sum)
(eval (list (car sum) (cadr sum) (caddr sum)))


  