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

cellToChar :: Cell -> Char
cellToChar cell 
	| cell == EmptyCell = emptyCell
	| cell == HorPipe = horPipe
	| cell == VerPipe = verPipe
	| cell == TopLeft = topLeft
	| cell == BotLeft = botLeft
	| cell == BotRight = botRight
	| cell == TopRight = topRight
	| cell == EmptySpace = emptySpace
	| cell == StartUp = startUp
	| cell == StartDown = startDown
	| cell == StartLeft = startLeft
	| cell == StartRight = startRight
	| cell == WinUp = winUp
	| cell == WinDown = winDown
	| cell == WinLeft = winLeft
	| otherwise = winRight	


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
createLevel right_corner list = foldr (\pair level -> addCell pair level) (emptyLevel right_corner) list


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

--functie care intoarce true/false daca celula ar putea fi mutata intr-o anumita directie 
--aici nu se valideaza mutarea, doar se verifica faptul ca pozitia pe care s-ar face mutarea este una care nu iese din matrice
checkNextPos :: Position -> Directions -> Level -> Bool
checkNextPos pos direction (LvlConst highterLeft lowerRight table)
	| (direction == North && ((fst pos) - 1) < 0) = False
	| (direction == South && ((fst pos) + 1) > (fst lowerRight)) = False
	| (direction == West && ((snd pos) - 1) < 0) = False
	| (direction == East && ((snd pos) + 1) > (snd lowerRight)) = False
	| otherwise = True

--intoarce valoarea acelei celule aflata pe directia indicata, langa celula cu pozitia data
getNeighbour :: Position -> Directions -> Level -> Cell
getNeighbour pos direction (LvlConst highterLeft lowerRight table)
	| direction == North = (table T.! (((fst pos) - 1), (snd pos)))
	| direction == South = (table T.! (((fst pos) + 1), (snd pos)))
	| direction == West = (table T.! ((fst pos), ((snd pos) - 1)))
	| direction == East = (table T.! ((fst pos), ((snd pos) + 1)))

--functie care intoarce pozitia (tip Position) pe care se doreste mutarea unei celule
getNextPos :: Position -> Directions -> Position
getNextPos pos direction
	| direction == North = (((fst pos) - 1), (snd pos))
	| direction == South = (((fst pos) + 1), (snd pos))
	| direction == West = ((fst pos), ((snd pos) - 1))
	| direction == East = ((fst pos), ((snd pos) + 1))


--functie care primeste un level si intoarce Celula (sub forma tipului Cell) de la o anumita pozitie din matrice
getCellFromPos :: Level -> Position -> Cell
getCellFromPos (LvlConst highterLeft lowerRight table) pos 
	| (((fst pos) > (fst lowerRight)) || ((snd pos) > (snd lowerRight))) = EmptyCell
	| (((fst pos) < 0) || ((snd pos) < 0)) = EmptyCell
	| otherwise = (table T.! pos)

--functie care primeste un level, o pozitie si o celula si inlocuieste elementul de la acea pozitie cu o anumita valoare
changeCell :: Position -> Cell -> Level -> Level
changeCell pos my_cell (LvlConst highterLeft lowerRight table) = (LvlConst highterLeft lowerRight (table T.// [(pos, my_cell)]))

--urmatoarele 2 functii intorc true/false daca celula data prin pozitie este sau nu celula de start, respectiv celula de end
isStartCell :: Position -> Level -> Bool
isStartCell pos level 
	| (cellToChar (getCellFromPos level pos)) `elem` startCells = True
	| otherwise = False

isEndCell :: Position -> Level -> Bool
isEndCell pos level 
	| (cellToChar (getCellFromPos level pos)) `elem` winningCells = True
	| otherwise = False



--verifica daca se poate face mutarea unei celule de la o pozitie data, intr-o anumita directie
moveCell :: Position -> Directions -> Level -> Level
moveCell position direction level 
	| (isStartCell position level) == True = level
	| (isEndCell position level) == True = level
	| (checkNextPos position direction level) == False = level --pozitia pe care s-at muta iese din matrice
	| (getNeighbour position direction level) /= EmptySpace = level --pozitia pe care vrea sa se mute nu este libera
	| otherwise = (changeCell position EmptySpace (addCell ((cellToChar (getCellFromPos level position)), dest_pos) level))
	where dest_pos = getNextPos position direction

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

--functie care intoarce o lista cu toate coordonatele de pe tabla
getAllPositions :: Level -> [Position]
getAllPositions (LvlConst highterLeft lowerRight table) = generatePairs (fst lowerRight) (snd lowerRight)


--intoarce pozitia pe tabla a celului de start(o celula din startCells)
getStartPos :: Level -> [Position] -> Position
getStartPos level positions 
	| isStartCell current_pos level == True = current_pos
	| otherwise = getStartPos level (tail positions) 
	where current_pos = head positions

--intoarce pozitia pe tabla a celului de win(o celula din winningCells)
getEndPos :: Level -> [Position] -> Position
getEndPos level positions 
	| isEndCell current_pos level == True = current_pos
	| otherwise = getEndPos level (tail positions) 
	where current_pos = head positions

--functie care primeste o celula si intoarce o lista de formata din alte 4 liste 
--fiecare lista din lista returnata contine celule cu care se poate conecta celula data pe fiecare din cele 4 directii
--EmptySpace -> nu exista celula care se poate conecta in acea directie
--[[N], [S], [W], [E]]
getCompatibleCells :: Cell -> [[Cell]]
getCompatibleCells cell 
	| cell == HorPipe = [[], [], [HorPipe, TopLeft, WinRight], [HorPipe, TopRight, WinLeft, BotRight]]
	| cell == VerPipe = [[VerPipe, TopLeft, TopRight, WinDown], [VerPipe, BotLeft, WinUp, BotRight], [], []]
	| cell == TopLeft = [[WinDown], [VerPipe, BotLeft, BotRight, WinUp], [], [HorPipe, TopRight, BotRight]]
	| cell == BotLeft = [[VerPipe, TopRight, HorPipe, WinDown], [], [], [HorPipe, BotRight, TopRight, WinLeft]]
	| cell == TopRight = [[], [VerPipe, BotLeft, BotRight, WinUp], [HorPipe, TopLeft, WinRight], []]
	| cell == BotRight = [[VerPipe, TopLeft, TopRight, WinDown], [], [HorPipe, TopLeft, WinRight, BotLeft], []]
	| cell == StartUp = [[VerPipe, TopRight, TopLeft], [], [], []]
	| cell == StartDown = [[], [VerPipe, TopRight, TopLeft],[], []]
	| cell == StartLeft = [[], [], [HorPipe, TopLeft, BotLeft], []]
	| cell == StartRight = [[], [], [], [HorPipe, TopRight, BotRight]]
	| otherwise = [[], [], [], []]

--primeste o directie si o celula si returneaza lista cu pipeurile care se pot conecta in directia data
getCompDirection :: Cell -> Directions-> [Cell]
getCompDirection cell dir 
	| dir == North = head (getCompatibleCells cell)
	| dir == South = head (tail (getCompatibleCells cell))
	| dir == West = head (tail (tail (getCompatibleCells cell)))
	| dir == East = head (tail (tail (tail (getCompatibleCells cell))))

connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 direction
	| cell2 `elem` getCompDirection cell1 direction  = True
	| otherwise = False


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

--functie care intoarce directia in care pozitia de start asteapta o conectivitate
--primeste tabla de joc
getStartDirection :: Level -> Directions
getStartDirection level
	| start_cell == StartUp = North
	| start_cell == StartDown = South
	| start_cell == StartLeft = West
	| start_cell == StartRight = East
	where start_cell = getCellFromPos level (getStartPos level (getAllPositions level)) --celula de start(tip Cell)

getEndDirection :: Level -> Directions
getEndDirection level
	| end_cell == WinUp = North
	| end_cell == WinDown = South
	| end_cell == WinLeft = West
	| end_cell == WinRight = East
	where end_cell = getCellFromPos level (getEndPos level (getAllPositions level)) --celula de win(tip Cell)

--primeste o celula si directia de conectare a ei cu celula precedenta 
--intoarce celula cu care se va conecta si directia de conectare asociata
nextCell :: (Position, Position) -> Level -> Position
nextCell (pos, old_pos) level  
--se verifica pe rand vecinii celului curente
	| ((connection current_cell cell_South South) && (old_pos /= pos_South)) = pos_South
	| (((snd pos) - 1 >= 0) && (connection current_cell cell_West West) && (old_pos /= pos_West)) = pos_West
	| (((fst pos) - 1 >= 0) && (connection current_cell cell_North North) && (old_pos /= pos_North)) = pos_North
	| ((connection current_cell cell_East East) && (old_pos /= pos_East)) = pos_East
	| otherwise = (-1, -1)
	where 
		current_cell = getCellFromPos level pos
		cell_North = getCellFromPos level ((fst pos) - 1, snd pos) 
		cell_South = getCellFromPos level ((fst pos) + 1, snd pos) 
		cell_East = getCellFromPos level ((fst pos), (snd pos) + 1) 
		cell_West = getCellFromPos level ((fst pos), (snd pos) - 1) 
		pos_South = ((fst pos) + 1, snd pos)
		pos_West = (fst pos, (snd pos) - 1)
		pos_North = ((fst pos) - 1, snd pos)
		pos_East = (fst pos, (snd pos) + 1)
	
{-	| ((direction == West) && (connection current_cell cell_West West)) = ((fst pos, (snd pos) - 1), West)
	| ((direction == East) && (connection current_cell cell_East East)) = ((fst pos, (snd pos) + 1), East)
	| otherwise = ((-1, -1), West) -}

--primeste ca parametri o celula(un pipe) si intoarce recursiv urmatoarea celula de pe tabela, vecina cu celula primita, care se poate conecta cu aceasta, pana cand celula obtinuta este de tipul Win sau nu este un pipe
checkPipesTunnel :: (Position, Position) -> Level -> Bool
checkPipesTunnel (cell_pos, old_pos) level 
	| isEndCell cell_pos level = True 
	| cell_pos == (-1, -1) = False
	| otherwise = checkPipesTunnel ((nextCell (cell_pos, old_pos) level), cell_pos) level


wonLevel :: Level -> Bool
wonLevel level 
--primele trei verificari valideaza ca celula care se conecteaza direct cu start sau end sa fie un pipe
--in aceste trei cazuri, se intoarce False daca celula conectata cu start\end nu este un pipe
	| getNeighbour start_pos start_direction level == EmptyCell = False
	| getNeighbour start_pos start_direction level == EmptySpace = False
	| getNeighbour end_pos end_direction level == EmptySpace = False
	| getNeighbour end_pos end_direction level == EmptyCell = False
	| otherwise = checkPipesTunnel ((getNextPos start_pos start_direction), start_pos) level

	where 
		start_direction = getStartDirection level --directia de start(data prin celula de start)
		start_pos = getStartPos level (getAllPositions level)
		end_direction = getEndDirection level --directia de start(data prin celula de start)
		end_pos = getEndPos level (getAllPositions level)
		start_pipe_pos = getNeighbour start_pos start_direction level --pipe-ul vecin lui startCell in directia in care se poate face o conexiune


--instanta a clasei ProblemState avand tipurile Level si (Position, Direction)
instance ProblemState Level (Position, Directions) where
	--va returna o lista de perechi de tipul ((pos, dir), another_state)
	--fiecare stare another_state este echivalenta cu a face o singura mutate in nivelul curent
	--adica se ava aplica (moveCell pos dir level) pentru fiecare celula, in fiecare directie 
	--rezultatul intoars de moveCell este adaugat in vectorul final daca starea another_state difera de cea curenta
	--se aplica asta pe o lista care contine toate pozitiile din level
    successors level = (filter (diffFunction level) (map function ((generateInput level North)))) ++
					   (filter (diffFunction level) (map function ((generateInput level South)))) ++
 					   (filter (diffFunction level) (map function ((generateInput level West)))) ++
					   (filter (diffFunction level) (map function ((generateInput level East))))
		where 
			function = ((\(position, level, direction) -> ((position, direction), (moveCell position direction level))))
		  
		
	--isGoal EmptyLevel = False
    isGoal level = wonLevel level
	
	


    reverseAction = undefined

--tipul starii este un level
--tipul actiunii: (pozitie de pe tabla, directie)

getLevel :: ((Position, Directions), Level) -> Level
getLevel pair = snd pair


--intoarce true daca level-urile sunt diferite
diffFunction :: Level -> ((Position, Directions), Level) -> Bool
diffFunction level1 pair = level1 /= (snd pair)


generateInput :: Level -> Directions -> [(Position, Level, Directions)]
generateInput level direction= [(elem, level, direction) | elem <- pos]
	where
		pos = getAllPositions level








