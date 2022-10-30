{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Search where

import Data.Maybe

import ProblemState
{-
    *** TODO ***

    Tipul unui nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node
  { state :: s,
    action :: Maybe a,
    parent :: Maybe (Node s a),
    children :: [Node s a],
    depth :: Int
  }

instance Eq s => Eq (Node s a)
    where x == y = state x == state y

instance Show s => Show (Node s a)
    where show node = show (state node)
{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState = state

nodeAction :: Node s a -> Maybe a
nodeAction = action

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

nodeDepth :: Node s a -> Int
nodeDepth = depth

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace level = createStateSpace_aux level Nothing Nothing 0 [level]

createStateSpace_aux :: (ProblemState s a, Eq s) => s -> Maybe a -> Maybe (Node s a) -> Int -> [s] -> Node s a
createStateSpace_aux level act par dpt visited = let succs = filter (\pair -> not $ elem (snd pair) visited) (successors level)
                                                     new_visited = foldl (\acc pair -> (acc ++ [snd pair])) visited succs
                                                     ch_parent = Node level act par [] dpt
                                                     ch = if dpt == 4 then [] -- depth limit
                                                          else map (\pair -> createStateSpace_aux (snd pair) (Just (fst pair)) (Just ch_parent) (dpt + 1) new_visited) succs
                                                     in Node level act par ch dpt


{-
    *** TODO ***

    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs start = [([start], [start])] ++ (bfs_stream [start])

bfs_aux :: Ord s => [Node s a] -> ([Node s a], [Node s a]) -- returns (new, frontier)
bfs_aux frontier = if null frontier then ([], [])
                   else let ch = children $ head frontier
                            in (ch, (tail frontier) ++ ch)

bfs_stream :: Ord s => [Node s a] -> [([Node s a], [Node s a])] -- returns [(new, frontier)]
bfs_stream frontier = if null frontier then []
                      else let pair = bfs_aux frontier
                               in [pair] ++ (bfs_stream $ snd pair)


{-
    *** TODO ***

    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

-- (using depth 4)
bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = let forward = bfs start
                         backward = bfs end
                         in let common_elem = commonElem (fst $ head forward) (fst $ head backward)
                                in if common_elem /= Nothing then fromJust common_elem
                                   else firstIntersection 1 forward backward

firstIntersection :: Ord s => Int -> [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
firstIntersection idx forward backward = let common_elem = commonElem (fst $ forward !! idx) (fst $ backward !! idx)
                                             in if common_elem /= Nothing then fromJust common_elem
                                                else let common_elem = commonElem (fst $ forward !! idx) (snd $ backward !! (idx - 1))
                                                         in if common_elem /= Nothing then fromJust common_elem
                                                            else firstIntersection (idx + 1) forward backward

commonElem :: Eq a => [a] -> [a] -> Maybe (a, a)
commonElem l1 l2 = if null l1 then Nothing
                   else let found = searchElem (head l1) l2
                            in if found /= Nothing then found
                            else commonElem (tail l1) l2

searchElem :: Eq a => a -> [a] -> Maybe (a, a)
searchElem e l = if null l then Nothing
                 else if e == head l then Just (e, head l)
                 else searchElem e (tail l)


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node = reverse $ map (\n -> (action $ fromJust n, state $ fromJust n))
                                 (takeWhile (not . isNothing) (iterate (\maybe_node -> if isNothing maybe_node then Nothing
                                                                                       else let par = parent $ fromJust maybe_node
                                                                                                in if isNothing par then Nothing
                                                                                                   else par) (Just node)))


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor

solve initial_level solved_level = let initial_state = createStateSpace initial_level
                                       solved_state = createStateSpace solved_level
                                       intersection = bidirBFS initial_state solved_state
                                       direct_path = extractPath $ fst intersection
                                       reverse_path = reverse $ extractPath $ snd intersection
                                       in direct_path ++ map (\pair -> let reversed = reverseAction (fromJust $ fst pair, snd pair)
                                                                           in (Just $ fst reversed, snd reversed))
                                                                           (init reverse_path)
