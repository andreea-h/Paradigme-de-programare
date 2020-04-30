{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare; 
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

--nod dintr-o cale
--informatii despre cum s-a ajung in acea stare
--o actiune

data Node s a = UndefinedNode | NodeConst { state :: s, --stare ->(level)
				      move_action :: Maybe a, --actiunea ->(position, direction)
                                      parent :: Node s a, --nod parinte
                                      depth :: Int, --adancime
	                              kids :: [Node s a]} deriving Eq
						
{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

--intorc campurile punctuale din nod
nodeState :: Node s a -> s
nodeState (NodeConst st _ _ _ _) = st
--nodeState (VidNode st) = (VidNode st)
--nodeState (VidNode v) = NodeConst VidNode v

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (NodeConst _ _ p _ _) = Just p
nodeParent UndefinedNode = Nothing
--cate mutari au fost necesare pentru a ajunge la acel nod
nodeDepth :: Node s a -> Int
nodeDepth (NodeConst _ _ _ d _) = d
nodeDepth UndefinedNode = 0

nodeAction :: Node s a -> Maybe a
nodeAction (NodeConst _ action _ _ _) = action
nodeAction UndefinedNode = Nothing

--in loc sa facem generarea in paralel
nodeChildren :: Node s a -> [Node s a]
nodeChildren (NodeConst _ _ _ _ k) = k
nodeChildren UndefinedNode = []

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}
--tipul s si a sunt inrolate in clasa ProblemState
--succesorii vor fi nodurile la care se poate ajunge intr-o singura mutare
createSuccessors :: (ProblemState s a) => a -> s -> Node s a -> Int -> Node s a
createSuccessors action my_state parent depth  = newNode
	where 
		newNode = NodeConst my_state (Just action) parent depth nextSucc
		nextSucc = map (\(an_action, a_state) -> createSuccessors an_action a_state newNode (depth + 1)) (successors my_state)

--crearea spatiului starilor
--pleaca de o stare initiala s
--construieste un arbore in care copiii unui nod sunt succesorii imediat
--copiii sunt nodurile in care se poate ajunge din nodul curent printr-o singura mutare
--Node s a este radacina acestui arbore generat
--starile trebuie marcate vizitate
--aspect al ciclurilor si starilor repetate a->b->a apare in spatiul starilor
--evitate a ciclurilor: ele apar in state Space
--abordam ciclurile in bfs

--copiii unui nod sunt succesorii lui imediati
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace init_state = thisNode
	where 
		thisNode = NodeConst init_state Nothing UndefinedNode 0 kids
		kids = map (\(an_action, a_state) -> createSuccessors an_action a_state thisNode 1) (successors init_state)
--kids : starile in care se poate ajunge facand o actiune in starea curenta
{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}


--lista este consstruita lenes
--la bfsdir cand se fac cele 2 bfs uri
--0->1->3
 --  ->2
--initial frontiera este goala/sau nodul de la care am plecat
--bfs 0 = [([0], [0]), ([1,2], [1,2]), ([3], [2,3]), ([], [3]), ([],[])]
--luam nodul care trebuie expandat (din partea a doua a perechii)
--cum vedem ca nu vizitam noduri deja vizitate?
--data.Set? sau lista pentru a retine nodurile deja vizitate
--cand adaugam un nod nou in frontiera, tre sa verificam daca nu a fost adaugam deja in lista; un functie ia frontiera returnata de bfs

--toate fromele pe care le poate lua frontiera
--lista se construieste pe masura ce o exploram efectiv

--ce returneaza este un stream care arata evolutia frontierei in timp
--un astfel de stream este tottalitatea formelor pe care le-a luat frontiera
--o pereche arata cum arata frontiera la un anumit pas
--a doua compeneta : nodurile la care s-a ajung dar care nu au fost expanadate
--nodurile care au fost adaugate in frontiera proastam 
bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs = undefined

--un bfs care pleaca din starea intitiala si unul care pleaca din starea finala

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}


--


--se cauta nodurile proaspat adaugate in frontira unui bfs in frontiera celuilalt bfs
--adica ma uit la al doilea element din ce intoarce bfs
---bfs1 -> [(neighbors1, frontiera1)
---bfs2 -> [(neighbors2, frontiera2) se cauta incrucisat n1 in f2 sau n2 in f1
--daca macar in una dintre cautari s-a gasit un element comun

--se fac 2 bfs in paralel
--unul din nodul de start , unul din final
--se opreste cand adaugam in frontiera noduri care se gasesc in frontiera celuilui bfs
--sau viceversa  -> cele 2 bfs uri s-au intersectat
--se construieste calea de la starea initiala la starea finala
bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined

--se dau ca paraemtri starea initiala, respectiv cea finala
--intoarce starea finala la care s-a ajuns in urma celor 2 bfs-uri
--cele 2 noduri din perechea intoarsa: primul contine drumul de la starea intiala la starea comuna, al doilea surprinde drumul de la starea finala la cea comuna
{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}
--ia un nod la care s-a ajuns si intorce drumul facutpentru a ajunge in starea aia
--starea de plecare: actiune Nothing
--intoarce sirul de actiuni de la parinte la copiii
--


extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}


--face extract pe cele 2 jumatati 
solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined

--la al doilea bfs se pleaca din starea finala
--SI -> ....... -> s <- .... <- SF undeva trebuie inversate actiunile
--SI -> ....... -> s -> .... -> SF



