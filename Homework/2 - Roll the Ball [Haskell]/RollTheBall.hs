{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState

import Data.Array


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
data Cell = Cell
  { code :: Char
  } deriving (Eq, Ord)

mutableCell :: Cell -> Bool
mutableCell cell = let c = code cell in if c == startUp || c == startDown || c == startLeft || c == startRight || c == winUp || c == winDown || c == winLeft || c == winRight || c == emptySpace then False
                                        else True

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level
  { cells :: Array Int Cell,
    height :: Int,
    width :: Int,
    dim :: Int
  } deriving (Eq, Ord)
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

instance Show Cell
    where show = \cell -> [code cell]

instance Show Level
    where show = \level -> endl : foldr (\i -> if (i + 1) `mod` (width level) == 0 then (((show $ (cells level) ! i) ++ [endl]) ++)
                                               else ((show $ (cells level) ! i) ++))
                                                  [] [0..(dim level) - 1]

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = let h = (fst pos) + 1
                     w = (snd pos) + 1
                     d = h * w
                     in Level (array (0, d - 1) ([(i, (Cell emptySpace)) | i <- [0 .. d - 1]])) h w d

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
addCell (c, pos) level = let idx = (width level) * (fst pos) + (snd pos)
                             in if fst pos < 0 || fst pos >= (height level) || snd pos < 0 || snd pos >= (width level) then level
                                else if code ((cells level) ! idx) == emptySpace then Level ((cells level) // [((width level) * (fst pos) + (snd pos), (Cell c))]) (height level) (width level) (dim level)
                                     else level


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
createLevel pos cells_list = foldr addCell (emptyLevel pos) cells_list


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir level = let x = fst pos
                             y = snd pos
                             idx = w * x + y
                             w = width level
                             d = dim level
                             arr = cells level
                             c = code $ arr ! idx
                             in if not $ validMove pos dir level then level
                                else if dir == North then Level (arr // [(idx - w, Cell c), (idx, Cell emptySpace)]) (height level) w d
                                else if dir == South then Level (arr // [(idx + w, Cell c), (idx, Cell emptySpace)]) (height level) w d
                                else if dir == West then Level (arr // [(idx - 1, Cell c), (idx, Cell emptySpace)]) (height level) w d
                                else if dir == East then Level (arr // [(idx + 1, Cell c), (idx, Cell emptySpace)]) (height level) w d
                                else level

validMove :: Position -> Directions -> Level -> Bool
validMove pos dir level = let x = fst pos
                              y = snd pos
                              idx = w * x + y
                              w = width level
                              arr = cells level
                              in if not . mutableCell $ arr ! idx then False
                                 else if dir == North then idx >= w && code (arr ! (idx - w)) == emptySpace
                                 else if dir == South then idx + w < (dim level) && code (arr ! (idx + w)) == emptySpace
                                 else if dir == West then idx `mod` w /= 0 && code (arr ! (idx - 1)) == emptySpace
                                 else if dir == East then (idx + 1) `mod` w /= 0 && code (arr ! (idx + 1)) == emptySpace
                                 else False

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}
connection :: Cell -> Cell -> Bool
connection cell1 cell2 = let c1 = code cell1
                             c = code cell2
                             in if c1 == horPipe then c == horPipe || c == topLeft || c == botLeft || c == botRight || c == topRight || c == winLeft || c == winRight
                                else if c1 == verPipe then c == verPipe || c == topLeft || c == botLeft || c == botRight || c == topRight || c == winUp || c == winDown
                                else if c1 == topLeft then c == horPipe || c == verPipe || c == botLeft || c == botRight || c == topRight || c == winUp || c == winLeft
                                else if c1 == botLeft then c == horPipe || c == verPipe || c == topLeft || c == botRight || c == topRight || c == winDown || c == winLeft
                                else if c1 == botRight then c == horPipe || c == verPipe || c == topLeft || c == botLeft || c == topRight || c == winDown || c == winRight
                                else if c1 == topRight then c == horPipe || c == verPipe || c == topLeft || c == botLeft || c == botRight || c == winUp || c == winRight
                                else if c1 == startUp then c == verPipe || c == topLeft || c == topRight || c == winDown
                                else if c1 == startDown then c == verPipe || c == botLeft || c == botRight || c == winUp
                                else if c1 == startLeft then c == horPipe || c == topLeft || c == botLeft || c == winRight
                                else if c1 == startRight then c == horPipe || c == botRight || c == topRight || c == winLeft
                                else False

cellsDirection :: Cell -> Cell -> [Directions] -- compatible directions
cellsDirection cell1 cell2 = let c1 = code cell1
                                 c2 = code cell2
                                 in if c1 == horPipe then if c2 == horPipe then [West, East]
                                                          else if c2 == topLeft || c2 == botLeft || c2 == winRight then [West]
                                                          else if c2 == botRight || c2 == topRight || c2 == winLeft then [East]
                                                          else []
                                    else if c1 == verPipe then if c2 == verPipe then [North, South]
                                                               else if c2 == topLeft || c2 == topRight || c2 == winDown then [North]
                                                               else if c2 == botLeft || c2 == botRight || c2 == winUp then [South]
                                                               else []
                                    else if c1 == topLeft then if c2 == botRight then [South, East]
                                                               else if c2 == horPipe || c2 == topRight || c2 == winLeft then [East]
                                                               else if c2 == verPipe || c2 == botLeft || c2 == winUp then [South]
                                                               else []
                                    else if c1 == botLeft then if c2 == topRight then [North, East]
                                                               else if c2 == horPipe || c2 == botRight || c2 == winLeft then [East]
                                                               else if c2 == verPipe || c2 == topLeft || c2 == winDown then [North]
                                                               else []
                                    else if c1 == botRight then if c2 == topLeft then [North, West]
                                                                else if c2 == horPipe || c2 == botLeft || c2 == winRight then [West]
                                                                else if c2 == verPipe || c2 == topRight || c2 == winDown then [North]
                                                                else []
                                    else if c1 == topRight then if c2 == botLeft then [South, West]
                                                                else if c2 == horPipe || c2 == topLeft || c2 == winRight then [West]
                                                                else if c2 == verPipe || c2 == botRight || c2 == winUp then [South]
                                                                else []
                                    else if c1 == startUp then if c2 == verPipe || c2 == topLeft || c2 == topRight || c2 == winDown then [North]
                                                               else []
                                    else if c1 == startDown then if c2 == verPipe || c2 == botLeft || c2 == botRight || c2 == winUp then [South]
                                                                 else []
                                    else if c1 == startLeft then if c2 == horPipe || c2 == topLeft || c2 == botLeft || c2 == winRight then [West]
                                                                 else []
                                    else if c1 == startRight then if c2 == horPipe || c2 == botRight || c2 == topRight || c2 == winLeft then [East]
                                                                  else []
                                    else []


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel lvl = let wonLevel_aux level idx = let arr = cells level
                                                c = code (arr ! idx)
                                                in if c == startUp || c == startDown || c == startLeft || c == startRight then won level idx (-1)
                                                   else wonLevel_aux level (idx + 1) -- if idx == dim level then False (stop condition if there might be no starting cell)
                   in wonLevel_aux lvl 0


won :: Level -> Int -> Int -> Bool -- level, current cell, previous cell
won level idx p = let arr = cells level
                      w = width level
                      cell = arr ! idx
                      c = code cell
                      idx_up = idx - w
                      idx_down = idx + w
                      idx_left = idx - 1
                      idx_right = idx + 1
                      in if c == winUp || c == winDown || c == winLeft || c == winRight then True
                         else if idx_up >= 0 && idx_up /= p && connection cell (arr ! idx_up) && elem North (cellsDirection cell (arr ! idx_up)) then won level idx_up idx
                         else if idx_down < (dim level) && idx_down /= p && connection cell (arr ! idx_down) && elem South (cellsDirection cell (arr ! idx_down)) then won level idx_down idx
                         else if idx `mod` w /= 0 && idx_left /= p && connection cell (arr ! idx_left) && elem West (cellsDirection cell (arr ! idx_left)) then won level idx_left idx
                         else if (idx + 1) `mod` w /= 0 && idx_right /= p && connection cell (arr ! idx_right) && elem East (cellsDirection cell (arr ! idx_right)) then won level idx_right idx
                         else False


instance ProblemState Level (Position, Directions) where
    successors level = foldr (\idx -> ((movesSuccessor idx level) ++)) [] [0 .. dim level - 1]
    isGoal = wonLevel
    reverseAction ((pos, dir), level) = if dir == North then let new_pos = (fst pos - 1, snd pos) in ((new_pos, South), moveCell new_pos South level)
                                        else if dir == South then let new_pos = (fst pos + 1, snd pos) in ((new_pos, North), moveCell new_pos North level)
                                        else if dir == West then let new_pos = (fst pos, snd pos - 1) in ((new_pos, East), moveCell new_pos East level)
                                        else if dir == East then let new_pos = (fst pos, snd pos + 1) in ((new_pos, West), moveCell new_pos West level)
                                        else ((pos, dir), level)

moveSuccessor :: Int -> Directions -> Level -> [((Position, Directions), Level)]
moveSuccessor idx dir level = let w = width level
                                  pos = (idx `div` w, idx `mod` w)
                                  in if validMove pos dir level then [((pos, dir), moveCell pos dir level)]
                                     else []
movesSuccessor :: Int -> Level -> [((Position, Directions), Level)]
movesSuccessor idx level = (moveSuccessor idx North level) ++ (moveSuccessor idx South level) ++ (moveSuccessor idx West level) ++ (moveSuccessor idx East level)
