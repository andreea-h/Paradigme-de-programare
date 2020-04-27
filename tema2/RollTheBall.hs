{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as T
import qualified Data.Set as S

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

--EmptySpace - celula neocupata
--EmptyCell - celula care nu este ocupata cu nimic (celula goala)

data Cell = EmptyCell | HorPipe | VerPipe | TopLeft | BotLeft | BotRight | TopRight | EmptySpace | 
		    StartUp | StartDown | StartLeft | StartRight | WinUp | WinDown |
		  	WinLeft | WinRight 
	deriving (Eq, Ord)


charToCell :: Char -> Cell
charToCell cell 
	| cell == emptyCell = EmptyCell
	| cell == horPipe = HorPipe
	| cell == verPipe = VerPipe
	| cell == topLeft = TopLeft
	| cell == botLeft = BotLeft
	| cell == botRight = BotRight
	| cell == topRight = TopRight
	| cell == emptySpace = EmptySpace
	| cell == startUp = StartUp
	| cell == startDown = StartDown
	| cell == startLeft = StartLeft
	| cell == startRight = StartRight
	| cell == winUp = WinUp
	| cell == winDown = WinDown
	| cell == winLeft = WinLeft
	| otherwise = WinRight	

instance Show Cell 
	where show my_cell 
		| my_cell == EmptyCell = [emptyCell]
		| my_cell == HorPipe =  [horPipe]
		| my_cell == VerPipe =  [verPipe]
		| my_cell == TopLeft = [topLeft]
		| my_cell == BotLeft =  [botLeft]
		| my_cell == BotRight =  [botRight]
		| my_cell == TopRight = [topRight]
		| my_cell == EmptySpace = [emptySpace]
		| my_cell == StartUp = [startUp]
		| my_cell == StartDown =  [startDown]
		| my_cell == StartLeft =  [startLeft]
		| my_cell == StartRight =  [startRight]
		| my_cell == WinUp =  [winUp]
		| my_cell == WinDown =  [winDown]
		| my_cell == WinLeft =  [winLeft]
		| my_cell == WinRight =  [winRight]


{-
    Tip de date pentru reprezentarea nivelului curent
-}

lowerRightCorner :: T.Array Position Cell -> Position 
lowerRightCorner  = snd . T.bounds

highterLeftCorner :: T.Array Position Cell -> Position 
highterLeftCorner = fst . T.bounds

elemAt :: (T.Array Position Cell) -> Position -> Cell
elemAt table pos = table T.! pos

data Level = EmptyLevel | LvlConst {
		highterLeft :: Position,
		lowerRight :: Position,
		table :: (T.Array Position Cell)
		}
  	 deriving (Eq, Ord)


{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

--afiseaza o linie din matrice formata dintr-un vector de celule
printLine :: Show a => [a] -> String
printLine cell_line = foldl (\res el -> res ++ (show el)) [endl] cell_line

--preia ca parametri matricea si o linie si intoarce o lista continand celulele de pe acea linie
--este folosita pt a putea reprezinta printr-o lista elementele de pe fiecare linie a matricei
getLineCells :: Level -> Int -> [Cell]
getLineCells (LvlConst highterLeft lowerRight table) line = [table T.! pos | pos <- (generatePairsX line (snd lowerRight))]


instance Show Level 
    where show EmptyLevel = ""
	  	  show (LvlConst highterLeft lowerRight table) = printed_table 
		where printed_table = (concatMap printLine
			[getLineCells (LvlConst highterLeft lowerRight table) line 
				| line <- [0..(fst lowerRight)]]) ++ [endl]

	

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

generatePairs x y = [(i, j) | i <- [0..x], j <- [0..y]]
generatePairsX x y = [(i, j) | j <- [0..y], i <- [x]]

emptyLevel :: Position -> Level
emptyLevel pos = LvlConst (0,0) pos (T.array ((0,0), pos) [(position, cell) | 
			position <- (generatePairs (fst pos) (snd pos)), cell <- [EmptySpace]])

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

--verifica daca o anumita pozitie de pe tabla este liber (este EmptyCell (adica este de tip EmptySpace))
--in caz afirmativ, intoarce True
checkAvailable :: Level -> Position -> Bool
checkAvailable EmptyLevel pos = True
checkAvailable (LvlConst highterLeft lowerRight table) pos =
	--case (((fst pos) > (fst lowerRight)) || ((snd pos) > (snd lowerRight))) of 
	--False -> False
	 (table T.! pos) == EmptySpace
	


--primeste ca argumente un level si 2 coordonate si verifica ca coordonatele sa nu fie in afara matricei
checkBounds :: Level -> Position -> Bool
checkBounds (LvlConst highterLeft lowerRight table) pos 
	| (fst pos) > nr_lines = False
	| (snd pos) > nr_columns = False
	| (fst pos) < 0 = False
	| (snd pos) < 0 = False
	| otherwise = True
	where
		nr_lines = fst lowerRight
		nr_columns = snd lowerRight
	


addCell :: (Char, Position) -> Level -> Level
addCell (my_cell, pos) EmptyLevel = LvlConst (0, 0) pos ((T.array ((0, 0), pos) 
 [(position, cell) | position <- (generatePairs (fst pos) (snd pos)), cell <- [EmptySpace]]) T.// [(pos, (charToCell my_cell))])

addCell (my_cell, pos) (LvlConst highterLeft lowerRight table) = 
	if ((checkBounds (LvlConst highterLeft lowerRight table) pos) == True)
		&& ((checkAvailable (LvlConst highterLeft lowerRight table) pos) == True) 
		then (LvlConst highterLeft lowerRight (table T.// [(pos, (charToCell my_cell))]))
		else (LvlConst highterLeft lowerRight table)
		


{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel right_corner list = 


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell = undefined

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection = undefined

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel = undefined

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
