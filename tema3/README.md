Tema are ca scop implementarea unui simplu agent conversațional bazat pe sistemul ELIZA. Dezvoltat în anii ‘60 s-a bucurat de multă popularitate fiind primul candidat cu șanse reale de a trece testul Turing la doar 14 ani după ce acesta a fost formulat.
In cadrului temei sunt abordate conceptele de baza din Prolog. Robotul este numit 'Gigel'.
Bazandu-ne pe o serie de reguli simple, va trebui ca pentru fiecare replică a utilizatorului sa fie produs un raspuns; acest răspuns va fi bazat pe replica utilizatorului, pe emoția detectată în conversație, și, in plus, pe subiectul detectat în conversație.

Exemplu de conversație:

    Tu:salut
    Gigel:salutare .
    Tu:ce faci ?
    Gigel:bine tu ?
    Tu:ma uit la un film
    Gigel:ce film ?
    Tu:ma uit la Trumbo
    Gigel:nu am auzit de trumbo , care este filmul tau favorit ?
    Tu:filmul meu favorit este Papillon
    Gigel:papillon ? nu am auzit de el , despre ce e vorba ?
    
Pentru a răspunde la o replică a utilizatorului, Gigel va folosi următorul algoritm:
 - va căuta în baza de cunoștințe setul de reguli pentru care toate cuvintele cheie se regăsesc în replica utilizatorului, în ordine.
 - în setul de reguli găsit, va găsi prima regulă care descrie corect forma replicii utilizatorului,
 - dintre răspunsurile posibile, dacă sunt mai multe, se va alege acel răspuns care a fost utilizat de mai puține ori de către Gigel în conversație, până acum. În acest scop se va folosi o memorie a replicilor de până acum ale lui Gigel.
 
 Gigel va selecta dintr-un set de reguli doar regulile care se potrivesc pentru emoția detectată a utilizatorului, dacă o astfel de indicație există în elementul de emoții din regulă.
 Emoția utilizatorului va fi detectată numărând aparițiile cuvintelor caracteristice fiecărei emoții.
 Gigel va selecta dintr-un set de reguli doar regulile care se potrivesc cu subiectul conversației, dacă o astfel de indicație există în elementul tags al regulii.Subiectul va fi detectat pe baza aparițiilor cuvintelor caracteristice fiecărui subiect.
 
