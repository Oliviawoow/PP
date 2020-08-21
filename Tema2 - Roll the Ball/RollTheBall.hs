{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East | NODIRECTION
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
--TODO
data Cell = Cell {caracter :: Char,
                    nDirection :: Directions,
                    sDirection :: Directions,
                    eDirection :: Directions,
                    vDirection :: Directions
                } deriving (Show, Ord)

instance Eq Cell where
    Cell x _ _ _ _ == Cell y _ _ _ _ = x == y


{-
    Tip de date pentru reprezentarea nivelului curent
-}
--TODO
data Level = Level {stare :: A.Array Position Cell}
    deriving (Ord)

instance Eq Level where
    Level x == Level y = x == y
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

instance Show Level where
    show (Level mat) = [endl] ++ (foldr (\x acum -> (foldr (\y acc -> [y] ++ acc) [] x) ++ [endl] ++ acum) [] matriceCaractere)
        where 
            -- coloane e nr de coloane din mat
            coloane = snd (snd (A.bounds mat))
            -- linii e nr de linii din mat
            linii = fst (snd (A.bounds mat))
            -- contine o lista de liste cu caractere
            matriceCaractere = [[caracter (mat A.! (x, y)) | y <- [0..coloane]] | x <- [0..linii]]

{- for debuggingss
arr::(A.Array (Int, Int) Cell)
arr = A.array ((0, 0), (1, 1)) 
              [((0, 0), (Cell topRight NODIRECTION South NODIRECTION West)), ((0, 1), (Cell emptySpace NODIRECTION NODIRECTION NODIRECTION NODIRECTION)),
               ((1, 0), (Cell emptySpace NODIRECTION NODIRECTION NODIRECTION NODIRECTION)), ((1, 1), (Cell emptySpace NODIRECTION NODIRECTION NODIRECTION NODIRECTION))]
-}

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}
-- [(x, y) | x <- [0..4], y <- [0..4]]

emptyLevel :: Position -> Level
emptyLevel p@(x, y) = Level (A.array ((0, 0), (x, y))
                            [((i, j), celulaGoala) | j <- [0..y], i <- [0..x]])
    where
        -- celula cu EmptySpace
        celulaGoala = Cell emptySpace NODIRECTION NODIRECTION NODIRECTION NODIRECTION

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

addCell :: (Char, Position) -> Level -> Level
addCell (t, p@(x, y)) level@(Level mat)
    | x < fst coltSusStanga || x > fst coltJosDreapta || y < snd coltSusStanga || y > snd coltJosDreapta = level
    | otherwise = Level (mat A.// [(p, celula)])
    where
        coltSusStanga = fst (A.bounds mat)
        coltJosDreapta = snd (A.bounds mat)
        celula = Cell t n s e v
        n 
            | elem t iesireNord == True = North
            | otherwise = NODIRECTION
        s 
            | elem t iesireSud == True = South
            | otherwise = NODIRECTION
        e
            | elem t iesireEst == True = East
            | otherwise = NODIRECTION
        v
            | elem t iesireVest == True = West
            | otherwise = NODIRECTION


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
createLevel poz lista = foldr (\x acc -> addCell x acc) (emptyLevel poz) lista


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell p@(x, y) dir nivel@(Level mat) 
    | x < fst coltSusStanga || x > fst coltJosDreapta || y < snd coltSusStanga || y > snd coltJosDreapta = nivel
    | elem celulaCaracter winningCells || elem celulaCaracter startCells = nivel
    | celulaCaracter == emptySpace = nivel
    | dir == North && x - 1 >= fst coltSusStanga && x - 1 <= fst coltJosDreapta && caracter (mat A.! (x - 1, y)) == emptySpace = Level (mat A.// [((x - 1, y), (mat A.! p)), (p, (mat A.! (x - 1, y)))])
    | dir == South && x + 1 >= fst coltSusStanga && x + 1 <= fst coltJosDreapta && caracter (mat A.! (x + 1, y)) == emptySpace = Level (mat A.// [((x + 1, y), (mat A.! p)), (p, (mat A.! (x + 1, y)))])
    | dir == East && y + 1 >= snd coltSusStanga && y + 1 <= snd coltJosDreapta && caracter (mat A.! (x, y + 1)) == emptySpace = Level (mat A.// [((x, y + 1), (mat A.! p)), (p, (mat A.! (x, y + 1)))])
    | dir == West && y - 1 >= snd coltSusStanga && y - 1 <= snd coltJosDreapta && caracter (mat A.! (x, y - 1)) == emptySpace = Level (mat A.// [((x, y - 1), (mat A.! p)), (p, (mat A.! (x, y - 1)))])
    | otherwise = nivel
    where
        coltSusStanga = fst (A.bounds mat)
        coltJosDreapta = snd (A.bounds mat)
        celulaCaracter = caracter (mat A.! p)

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell celula1 _ _ _ _) (Cell celula2 _ _ _ _) dir 
    | dir == North && elem celula1 iesireNord && elem celula2 iesireSud = True
    | dir == South && elem celula1 iesireSud && elem celula2 iesireNord = True
    | dir == East && elem celula1 iesireEst && elem celula2 iesireVest = True
    | dir == West && elem celula1 iesireVest && elem celula2 iesireEst = True
    | otherwise = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
-- [(x, y) | x <- [0..4], y <- [0..4]]

wonLevel :: Level -> Bool
wonLevel level@(Level mat) 
    | startTF == False = False
    | otherwise = celStartConect start level
    where
        coltDreapta = snd (A.bounds mat)
        -- pozitii posibile
        pozitii = [(x, y) | x <- [0..(fst coltDreapta)], y <- [0..(snd coltDreapta)]]
        -- pozitia celulei de start
        start = foldr (\ x acc -> if elem (caracter (mat A.! x)) startCells then x else acc) (-1, -1) pozitii
        -- a gasit celula de start
        startTF
            | start == (-1, -1) = False
            | otherwise = True

-- conexiuni cu restul celulelor
conexiune :: Position -> Level -> Directions -> Bool
conexiune poz@(x, y) nivel@(Level mat) dircame 
    | elem (caracter celula) winningCells = True
    | directieNouaValida && (connection celula (mat A.! directieNoua) directie) == True = conexiune directieNoua nivel dirCelConectt
    | otherwise = False
    where
        celula = mat A.! poz
        -- directia unde se poate conecta urmatoarea celula
        directie 
            | dircame == North && elem (caracter celula) iesireSud == True = South
            | dircame == North && elem (caracter celula) iesireEst == True = East
            | dircame == North && elem (caracter celula) iesireVest == True = West
            | dircame == South && elem (caracter celula) iesireNord == True = North
            | dircame == South && elem (caracter celula) iesireEst == True = East
            | dircame == South && elem (caracter celula) iesireVest == True = West
            | dircame == East && elem (caracter celula) iesireNord == True = North
            | dircame == East && elem (caracter celula) iesireSud == True = South
            | dircame == East && elem (caracter celula) iesireVest == True = West
            | dircame == West && elem (caracter celula) iesireNord == True = North
            | dircame == West && elem (caracter celula) iesireSud == True = South
            | dircame == West && elem (caracter celula) iesireEst == True = East
            | otherwise = NODIRECTION
        directieNoua
            | directie == North = (x - 1, y)
            | directie == South = (x + 1, y)
            | directie == East = (x, y + 1)
            | directie == West = (x, y - 1)
            | otherwise = (-1, -1)
        directieNouaValida = verificareMatrice directieNoua nivel
        dirCelConectt
            | directie == North = South
            | directie == South = North
            | directie == East = West
            | directie == West = East
            | otherwise = NODIRECTION

-- conexiune celula start
celStartConect :: Position -> Level -> Bool
celStartConect poz@(x, y) nivel@(Level mat) 
    | newPozValid && (connection celStart (mat A.! newPoz) dirCelStart) == True = conexiune newPoz nivel dirCelConect
    | otherwise = False
    where 
        celStart = mat A.! poz
        -- directia celulei de start
        dirCelStart 
            | nDirection celStart == North = North
            | sDirection celStart == South = South
            | eDirection celStart == East = East
            | vDirection celStart == West = West
            | otherwise = NODIRECTION
        -- pozitia urmatoarei celule
        newPoz 
            | dirCelStart == North = (x - 1, y)
            | dirCelStart == South = (x + 1, y)
            | dirCelStart == East = (x, y + 1)
            | otherwise = (x, y - 1)
        newPozValid = verificareMatrice newPoz nivel
        dirCelConect 
            | dirCelStart == North = South
            | dirCelStart == South = North
            | dirCelStart == East = West
            | dirCelStart == West = East
            | otherwise = NODIRECTION


-- daca pozitia se afla in matrice
verificareMatrice :: Position -> Level -> Bool
verificareMatrice poz@(x, y) (Level mat) 
    | x < fst coltSusStanga || x > fst coltJosDreapta || y < snd coltSusStanga || y > snd coltJosDreapta = False
    | otherwise = True
    where 
        coltSusStanga = fst (A.bounds mat)
        coltJosDreapta = snd (A.bounds mat)


instance ProblemState Level (Position, Directions) where
    successors level@(Level mat) = foldr(\ x acc -> (mutari x level) ++ acc) [] pozitiiEmpty
        where
            --celula = caracter (mat A.! )
            coltJosDreapta = snd (A.bounds mat)
            pozitii = [(i, j) | i <- [0..fst coltJosDreapta], j <- [0..snd coltJosDreapta]]
            pozitiiEmpty = foldr (\ x acc -> if (caracter (mat A.! x)) == emptySpace then x : acc else acc) [] pozitii
            --mutariPosibile = foldr(\ x acc -> (mutari x level) : acc) [] pozitiiEmpty

    isGoal level = wonLevel level

    reverseAction ((p@(x, y), dir), level@(Level mat)) = ((schimbaPozitia, schimbaDirectia), (moveCell schimbaPozitia schimbaDirectia level))
        where
            -- inapoi de unde a venit
            schimbaDirectia
                | dir == North = South
                | dir == South = North
                | dir == East = West
                | dir == West = East
                | otherwise = NODIRECTION
            -- pozitia initiala
            schimbaPozitia
                | dir == North = (x - 1, y)
                | dir == South = (x + 1, y)
                | dir == East = (x, y + 1)
                | dir == West = (x, y - 1)
                | otherwise = (-1, -1)

mutari :: Position -> Level -> [((Position, Directions), Level)]
mutari p@(x, y) level@(Level mat) = (if fst(fst moveDown) == (-1, -1) then [] else [moveDown]) ++ (if fst(fst moveUp) == (-1, -1) then [] else [moveUp]) ++ (if fst(fst moveLeft) == (-1, -1) then [] else [moveLeft]) ++ (if fst(fst moveRight) == (-1, -1) then [] else [moveRight])
    where
        -- vecinii
        sus = (x - 1, y)
        jos = (x + 1, y)
        dreapta = (x, y + 1)
        stanga = (x, y - 1)
        -- casuta de sus poate merge jos
        moveDown
            | (((verificareMatrice sus level) && elem (caracter (mat A.! sus)) oriceDarNuStartSiStop) == True) = ((sus, South), updatejos)
            | otherwise = (((-1, -1), NODIRECTION), level)
        -- casuta de jos poate merge sus
        moveUp
            | (((verificareMatrice jos level) && elem (caracter (mat A.! jos)) oriceDarNuStartSiStop) == True) = ((jos, North), updatesus)
            | otherwise = (((-1, -1), NODIRECTION), level)
        -- casuta din dreapta poate merge in stanga
        moveLeft
            | (((verificareMatrice dreapta level) && elem (caracter (mat A.! dreapta)) oriceDarNuStartSiStop) == True) = ((dreapta, West), updatedreapta)
            | otherwise = (((-1, -1), NODIRECTION), level)
        -- casuta din stanga poate merge in dreapta
        moveRight
            | (((verificareMatrice stanga level) && elem (caracter (mat A.! stanga)) oriceDarNuStartSiStop) == True) = ((stanga, East), updatestanga)
            | otherwise = (((-1, -1), NODIRECTION), level)
        updatejos = moveCell sus South level
        updatesus = moveCell jos North level
        updatestanga = moveCell stanga East level
        updatedreapta = moveCell dreapta West level

iesireNord :: [Char]
iesireNord = [verPipe, botLeft, botRight, startUp, winUp]

iesireSud :: [Char]
iesireSud = [verPipe, topLeft, topRight, startDown, winDown]

iesireEst :: [Char]
iesireEst = [horPipe, topLeft, botLeft, startRight, winRight]

iesireVest :: [Char]
iesireVest = [horPipe, botRight, topRight, startLeft, winLeft]

oriceDarNuStartSiStop :: [Char]
oriceDarNuStartSiStop = [verPipe, botLeft, botRight, topLeft, topRight, horPipe, emptyCell]
