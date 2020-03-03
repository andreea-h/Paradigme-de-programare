#lang racket


; apăsați pe Check Syntax și apoi puneți mouse-ul pe variabile
(define f (λ (a b) ; λ cu Ctrl-\
            (if a
                b
                ( (λ (a b) (cons a b)) 1 a)
                )))
         
; evens normal
(define evens-a (λ (L)
                  (if (null? L)
                      '()   ; null   ; (list)
                      (if (even? (car L))
                          (cons (car L) (evens-a (cdr L)))
                          (evens-a (cdr L))) ; duplic apelul pentru evens-a
                      )))

; utilizare append și introducere if mai în interior
; (de asemenea, recursivitate pe stivă)
(define evens-s (λ (L)
                  (if (null? L)
                      '()
                      (append
                       (if (even? (car L)) (list (car L)) null)
                       (evens-s (cdr L)))
                      )))

; exemplu legare folosind lambda:
; punem o parte din cod într-o funcție anonimă, pentru a putea 'salva' valoarea
;  întoarsă de (evens (cdr L)), pe care o pasăm ca parametru funcției anonime
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

; aceeași legare, dar folosind let
(define evens2 (λ (L)
                 (if (null? L)
                     '()
                     (let ((evenscdr (evens2 (cdr L))))
                       (if (even? (car L))
                           (cons (car L) evenscdr)
                           evenscdr)
                       )
                     )))


(let ((a 1) (b 2)) ; aceste legări nu sunt vizibile în *corpul* let-ului interior
  (let ((a 2) (b 3))
    (+ a b)
    ))

(let* [(a (+ 1 2))
       (b (* a a))
       (c (+ a b))
       ]
  c)

; evitare let*, foarte greoaie (implementare let* cu let)
(let ((a (+ 1 2)))
  (let ((b (* a a)))
    (let ((c (+ a b)))
      c)))


;test curs
; câte variabile există (și ce nume au) ?
; câte apariții are fiecare variabilă, și la ce linii? (în afară de definiția variabilei)

; legările din let sunt vizibile în corpul lui let
(define test (λ (x y) ; aici 3 variabile (nume), dacă includem pe test; în let încă 3 variabile
               (let* ((z x) ; apariție x din λ
                      (x x) ; apariție x din λ
                      (y x) ; apariție x din let*
                      )
                 (+ x y z) ; apariții x, y și z din let*
                 ))) ; y din λ nu apare


; funcție strictă
(define strict (λ (conditie x)
                 (if conditie x #f)))

; if este funcție nestrictă - (if #f (/ 5 0) 'ceva) nu va da eroare,
;  pentru că ramura pentru true nu este evaluată.

; La fel pentru and și or:
;(and #f (/ 5 0))
;(or #t (/ 5 0))


; definire factorial locală (numele va fi disponibil doar în letrec)
; recursiv pe stivă
(letrec ((fact-1
          (λ (n)
            (if (= n 1)
                1
                (* n (fact-1 (- n 1)))
                )
            )))
  (fact-1 5))


; folosiți Debug pentru a observa cum stiva nu se încarcă cu apeluri fact-aux
; după evaluarea lui (= n 1) cu n=1, întoarcerea se face direct la ieșirea din fact-2

; factorial recursiv pe coadă
(define fact-2 (λ (n)
                 (letrec ((fact-aux (λ (n rezultat)
                                      (if (= n 1)
                                          rezultat
                                          (fact-aux
                                           (- n 1)
                                           (* n rezultat))
                                          ))
                                    ))
                   (fact-aux n 1))))
(fact-2 5)

; evens recursiv pe coadă
(define evens-tail
  (λ (L)
    (letrec ((evens-aux (λ (L R)
                          (if (null? L) ; la terminarea recursivității
                              R         ; în RP avem deja rezultatul final
                              (evens-aux
                               (cdr L)  ; restul listei
                               ; construim rezultatul parțial
                               (if (even? (car L))
                                   (cons (car L) R)
                                   R))
                              ))))
      (reverse (evens-aux L '()))))) ; la final rezultatul întors va fi în ordine inversă
; pentru că primul element par din listă este adăugat cel mai devreme în RP, deci ultimul,
; pentru că celelalte elemente vor fi adăugat mereu la începutul listei

; la recursivitatea pe stivă, primul element par din listă este adăugat cel mai târziu
; (deci primul) în rezultat, după revenirea din recursivitate


; recursivitate pe coadă fără argument suplimentar, pentru evens
; trebuie să reținem și ce elemente mai avem de văzut, și rezultatul parțial până acum
; o soluție: folosim o pereche dintre cele 2, pe care o transmitem ca argument
(define evens-pair
  (λ (L)
    (letrec ((aux (λ (P)
                    (let ((L (car P)) (R (cdr P)))
                      (if (null? L)
                          (reverse R)
                          (aux (cons (cdr L)
                                     (if (even? (car L))
                                         (cons (car L) R)
                                         R)))
                          )))))
      (aux (cons L '())))))
; observație: doar "vedem" argumentul P ca pereche; el de fapt este o listă, având pe prima poziție lista L

; altă soluție: folosim un marker pentru "unde am ajuns" în listă
(define evens-marker
  (λ (L) (letrec ((aux (λ (A)
                         (let* ((pos (- (length A) (length (member 'M A))))
                                (R (take A pos))
                                (L (cdr (drop A pos)))
                                (LocalRes (if (and (not (null? L)) (even? (car L))) (list (car L)) null))
                                )
                           (display A) (display " -> ") (display R) (display L) (newline)
                           (if (null? L)
                               R
                               (aux (append R LocalRes (list 'M) (cdr L))))
                           ))))
           (aux (cons 'M L))
           )))

;(evens-marker '(1 2 3 4 5 6 7 8))
                           

; fără funcție auxiliară
; presupunem că inițial vom primi doar liste de numere
(define evens-A
  (λ (A)
    (if (member 'M A)
        (let* ((pos (- (length A) (length (member 'M A))))
               (R (take A pos))
               (L (cdr (drop A pos)))
               (LocalRes (if (and (not (null? L)) (even? (car L))) (list (car L)) null))
               )
          (display A) (display " -> ") (display R) (display L) (newline)
          (if (null? L)
              R
              (evens-A (append R LocalRes (list 'M) (cdr L))))
          )
        (evens-A (cons 'M A))
        )))

(evens-A '(1 2 3 4 5 6 7 8))




