#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "abilities.rkt")
(require "constants.rkt")
(require "random.rkt")

;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!
;Inițial state
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!

;TODO 1
; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului.

(define-struct struct-bird (x y v-y) #:transparent)
(define-struct struct-pipe (x y) #:transparent)
(define-struct struct-variables (gravity momentum scroll-speed) #:transparent)

;(define-struct struct-ability (image time pos next) #:transparent)

(define my_bird (struct-bird bird-x bird-initial-y 0))
(define my_pipe (struct-pipe scene-width (+ added-number (random random-threshold))))
(define my_variables (struct-variables initial-gravity initial-momentum initial-scroll-speed))
(define my_abilities_visible (list '() '()))

(define-struct struct-state (my_bird pipes-list my_variables score abilities) #:transparent)
(define pipes-list (list my_pipe))
;(define my_state (struct-state my_bird pipes-list my_variables 0 abilities))


;(struct-pipe-x my_pipe)
;(struct-bird-x my_bird)
;(struct-variables-momentum my_variables)

;TODO 8
; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!

;TODO 16
; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.
(define (get-initial-state)
 ; (struct-state my_bird pipes-list my_variables 0))
 ; my_state)
  (struct-state (struct-bird bird-x bird-initial-y 0) (list (struct-pipe scene-width (+ added-number (random random-threshold))))
                (struct-variables initial-gravity initial-momentum initial-scroll-speed) 0 null))
;TODO 2
; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.
(define (get-bird state)
 (struct-state-my_bird state))
 ; my_bird)

(define (get-bird-y bird)
  (struct-bird-y bird))
;(define coord (get-bird-y my_bird))

;TODO 3
; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.
;my_bird
(define (next-state-bird bird gravity)
  (let ((old-v (struct-bird-v-y bird))
       (old-pos (struct-bird-y bird)))
       (struct-copy struct-bird bird [y (+ old-pos old-v)] [v-y (+ old-v gravity)])))

;(next-state-bird my_bird 10)
             
              ;(struct-copy struct-bird bird [v-y (+ val gravity)])))

;TODO 4
; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.
(define (get-bird-v-y bird)
  (struct-bird-v-y bird))

;(get-bird-v-y my_bird)

;TODO 6
; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimbă viteza
; pe y a păsării cu acea valoare.
(define (next-state-bird-onspace bird momentum)
  (struct-copy struct-bird bird [v-y (* -1 momentum)]))

;(next-state-bird-onspace my_bird 20)

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
;TODO 7
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.
(define (change current-state pressed-key)
   (match-let ((current_bird (get-bird current-state)))
              (cond ((key=? pressed-key " ")
                     (struct-state (next-state-bird-onspace current_bird (get-variables-momentum (get-variables current-state)))
                                      (get-pipes current-state) (get-variables current-state) (get-score current-state) (get-abilities current-state)))
                     (else current-state))))
                   

;TODO 9
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.
(define (get-pipes state)
  (struct-state-pipes-list state))

;TODO 10
; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.
(define (get-pipe-x pipe)
  (struct-pipe-x pipe))

;TODO 11
; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va adaugă x-ului fiecărui pipe
; scroll-speed-ul dat.
(define (move-pipes pipes scroll-speed)
  (map (λ (pipe) ;;fiecare element din lista pipes este un struct-pipe
         ;intoarce un nou pipe modificat conform specificatiilor
        (match-let ([(struct-pipe x _) pipe])
          (struct-copy struct-pipe pipe [x (- x scroll-speed)])))
           pipes))


;TODO 12
; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.
;
; Hint: cunoaștem lățimea unui pipe, pipe-width
(define (clean-pipes pipes)
  (filter (λ (pipe) ;;pentru fiecare pipe din lista verifica daca mai este in ecran
             (if (<= (+ (struct-pipe-x pipe) pipe-width) 0) ;pipe-ul trebuie eliminat
                 #f
                 #t))
                 pipes))

(define (last-element pipes)
  (if (null? (cdr pipes))
      (car pipes)
      (last-element (cdr pipes))))
           
                     
;TODO 13
; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.
(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (append pipes (list (struct-pipe (+ (+ pipe-width pipe-gap) (struct-pipe-x (last-element pipes))) (+ added-number (random random-threshold)))))
      pipes))

;TODO 14
; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.
(define (next-state-pipes pipes scroll-speed)
      (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

;TODO 17
; Creați un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (struct-state-score state))

;TODO 19
; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.
;
; Hint: știm înălțimea păsării, bird-height, și y-ul pământului, ground-y.
; Coliziunea ar presupune ca un colț inferior al păsării să aibă y-ul
; mai mare sau egal cu cel al pământului.
(define (check-ground-collision bird)
   (let ((y-coord (get-bird-y bird)))
         (if (>= (+ y-coord bird-height) ground-y)
             #t ;coliziune cu pamantul
             #f)))

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

  

;TODO 20
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

;TODO 22
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.
(define (invalid-state? state)
  (or (check-ground-collision (get-bird state))
      (check-pipe-collisions (get-bird state) (get-pipes state))))

;TODO 21
; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
(define (get-near-pipe bird pipes) ;intoarce valoarea minima a diferentei dintre pipe-x si bird-x
  (apply min (map (λ (pipe)
                    (- (get-pipe-x pipe)  (struct-bird-x bird)))
                      pipes)))

(define (get-pipe-with-x x pipes bird) ;intoarce pipe-ul cu valoarea lui x data 
  (filter (λ (pipe)
             (if (= (- (get-pipe-x pipe) (struct-bird-x bird)) x)
                #t
                #f))
          pipes))

; Hint: Vă puteți folosi de check-collision-rectangle, care va primi drept parametri
; colțul din stânga sus și cel din dreapta jos ale celor două dreptunghiuri
; pe care vrem să verificăm coliziunea.
(define (check-pipe-collisions bird pipes)
   ; (let ((pipe (car (get-pipe-with-x (get-near-pipe bird pipes) pipes bird))))
  (ormap (λ (pipe)
            (or
            ;verifica coliziunea cu dreptunghiul de sus
            (check-collision-rectangles  (make-posn (struct-bird-x bird) (struct-bird-y bird))
                                    (make-posn (+ (struct-bird-x bird) bird-width) (+ (struct-bird-y bird) bird-height))
                                    (make-posn (get-pipe-x pipe) (- (struct-bird-y bird) pipe-height))
                                   (make-posn (+ (struct-pipe-x pipe) pipe-width) (struct-pipe-y pipe)))
            (check-collision-rectangles (make-posn (struct-bird-x bird) (struct-bird-y bird))
                                   (make-posn (+ (struct-bird-x bird) bird-width) (+ (struct-bird-y bird) bird-height))
                                   (make-posn (get-pipe-x pipe) (+ (struct-pipe-y pipe) pipe-self-gap))
                                   (make-posn (+ (get-pipe-x pipe) pipe-width) (+ (struct-bird-y bird) pipe-height)))))
         pipes))
        
(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.

;TODO 5
; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

;TODO 15
; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

;TODO 18
; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.
(define (next-state state)
  (match-let ((old_bird (get-bird state))
               (old_variables (get-variables state))
               (old_pipes (get-pipes state))
               (old_score (get-score state)))
  (struct-state (next-state-bird old_bird (get-variables-gravity old_variables))
                (next-state-pipes old_pipes (get-variables-scroll-speed old_variables)) (get-variables state) (+ old_score 0.1)
               ; (next-abilities (position-abilities ABILITIES) (get-bird state) 10 ))))
                (next-abilities-visible ABILITIES (get-variables-scroll-speed old_variables)))))
 ; (struct-state (next-state-bird (get-bird state) (get-variables-gravity (get-variables state)))
          ;      (next-state-pipes (get-pipes state) (get-variables-scroll-speed (get-variables state))) (get-variables state) 0))
 
  
; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
;(define initial-scene (empty-scene scene-width scene-height))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))
(define pipe-image (rectangle pipe-width pipe-height "solid" "green"))
(define gap-image (rectangle pipe-width pipe-self-gap "solid" "pink"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))


(define (draw-frame state)
 (match-let ([(struct-bird x y v-y) (get-bird state)])
 (place-visible-abilities (get-abilities state)
   (place-image bird-image (+ x (quotient bird-width 2))  (+ y (quotient bird-height 2))
             (place-image ground-image (quotient scene-width 2) (- scene-height (quotient ground-height 2))
                  (place-image (score-to-image (get-score state)) (posn-x text-posn) (posn-y text-posn)
                           (place-pipes (get-pipes state)
                              initial-scene)))))))
  ;(place-pipes (get-pipes state) initial-scene))
 
  
; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
(define (place-pipes pipes scene)
  (foldl (λ (pipe rezP)
             ;(place-image pipe-image (get-pipe-x pipe) (struct-pipe-y pipe) rezP))
           (place-images (list pipe-image pipe-image ;gap-image)
                               )
                         (list (make-posn (+ (get-pipe-x pipe) (quotient pipe-width 2))  (- (struct-pipe-y pipe) (quotient pipe-height 2)))
                               (make-posn (+ (get-pipe-x pipe) (quotient pipe-width 2)) (+ (+ (struct-pipe-y pipe) pipe-self-gap) (quotient pipe-height 2))))
                            rezP))
         scene pipes))
  

; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va accelera timpul va dura 10 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define fast-ability
 (struct-ability (hourglass "tomato") 10 null (λ (variables)
                                                 (match-let ((old_speed (get-variables-scroll-speed variables)))
                                                            (struct-copy struct-variables variables [scroll-speed (+ 1 old_speed)])))))

; Abilitatea care va încetini timpul va dura 30 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define slow-ability
   (struct-ability (hourglass "mediumseagreen") 30 null (λ (variables)
                                                (match-let ((old_speed (get-variables-scroll-speed)))
                                                           (struct-copy struct-variables variables [scroll-speed (max 5 (- old_speed 1))])))))

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list slow-ability fast-ability))


(define get-variables
  (λ (my_state)
    (struct-state-my_variables my_state)))

(define get-variables-gravity
  (λ (my_variables)
     (struct-variables-gravity my_variables)))
     
(define get-variables-momentum
  (λ (my_variables)
     (struct-variables-momentum my_variables)))

(define get-variables-scroll-speed
  (λ (my_variables)
     (struct-variables-scroll-speed my_variables)))

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities x)
  (struct-state-abilities x))
  
  

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define (get-abilities-visible x)
   (list (car x)))


; Întoarce abilităţile active din reprezentarea intermediară.
(define (get-abilities-active x)
  (cdr x))
  

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.
(define (clean-abilities abilities)
   (filter (λ (visible-ability) 
             (if (<= (posn-x (struct-ability-pos visible-ability)) 0) 
                 #f
                 #t))
                 abilities))


; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.
(define (move-abilities abilities scroll-speed)
  (map (λ (ability)
        (let ((x (posn-x (struct-ability-pos ability)))
              (y (posn-y (struct-ability-pos ability))))
              (struct-copy struct-ability ability (pos (make-posn (- x scroll-speed) y)))))
               abilities))



; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.
(define (time-counter abilities) ;;abilities este lista abilitatilor active
  (map (λ (ability) 
          (if (< (- (struct-ability-time ability) (/ 0.1 fps)) 0)
              #f ;adica se va sterge acea abilitate
              (struct-ability (get-ability-image) (- (struct-ability-time ability) (/ 0.1 fps)) (get-ability-pos) (get-ability-next)))
         abilities)))

; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..
(define (next-abilities-visible visible scroll-speed)
    (fill-abilities visible DISPLAYED_ABILITIES (clean-abilities (move-abilities (position-abilities visible) scroll-speed))))
  ;visible)

; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`
(define (next-abilities abilities bird scroll-speed)
  (append (next-abilities-visible abilities scroll-speed)
   (filter (λ (ability) ;verifica coliziunea pasarii cu abilitatea 
             (if (check-collision-rectangles
                  (make-posn (struct-bird-x bird) (struct-bird-y bird))
                  (make-posn (+ (struct-bird-x bird) bird-width) (+ (struct-bird-y bird) bird-height))
                  (get-ability-pos ability)
                  (make-posn (+ (posn-x (get-ability-pos ability)) (image-width (get-ability-image ability)))
                             (+ (posn-y (get-ability-pos ability)) (image-height (get-ability-image ability))))) ;;coliziune
                 #t
                 #f))
          abilities)))
                  
                                            

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când


(define (next-variables variables abilities)
  (map (λ (variable)
          ((compose-abilities abilities) variable))
          abilities))


; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
(define (place-visible-abilities abilities scene)
   (foldl (λ (ability rezP)
           (if (null? (get-ability-pos ability))
                   rezP
               (place-image (struct-ability-image ability) (posn-x (get-ability-pos ability)) (posn-y (get-ability-pos ability))
                                              rezP)))
           scene abilities))

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la ability-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (ability-posn.x - 50*i, ability-posn.y)
(define (place-active-abilities abilities scene)
  (foldl (λ (ability rezP)
            (place-image struct-ability-image (- (posn-x abilities-posn) (* 50 (index-of abilities ability))) (posn-y (abilities-posn)) rezP))
           abilities scene))
           

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))