Tema Racket - **Flappy Bird**

Tema are ca obiectiv utilizarea mecanismelor functionale ale limbajului Racket cu scopul de a crea o aplicatie vizuala interactiva.
Este folosita biblioteca *universe*.
In cadrul jocului, gravitatia actioneaza constant asupra pasarii. Astfel, la fiecare cadru care trece, vitezei pe y a pasarii i se adauga valoarea gravitatiei. Atunci cand vrem sa imprimam un impuls pasarii, viteza pe y a pasarii va fi inlocuita complet cu o valoare data.
Pentru a simplifica reprezentarea obiectelor in starea jocului, ne vom folosi de pozitia coltului din stanga sus al fiecarui obiect.
Reprezenatarea starii jocului la un moment dat include informatii despre:
* bird: pozitia pe verticala, viteaza pe verticala
* pipe: se retin informatii despre spatiul dintre 2 pipe-uri: pozitia gap-ului pe verticala, pozitia pe orizontala
* variables(valorile care pot varia in functie de abilitati): gravity, momentum, scroll-speed (cat de repede se misa obiectele pe scena)

![alt-text](https://github.com/andreea-h/Paradigme-de-programare/blob/master/Tema1-Racket:Flappy_Bird/elemente.png)

Pentru a determina daca pasarea atinge unul din pipe-uri verificam daca dreptunghiurile prin care se fac reprezentarile grafice se intersecteaza.

