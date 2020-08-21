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

data Node s a = Node {parinti :: Maybe (Node s a),
                kids :: [Node s a],
                stare :: s,
                actiune :: Maybe a,
                adancime :: Int}

instance Eq s => Eq (Node s a) where
    Node _ _ s1 _ _ == Node _ _ s2 _ _ = s1 == s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (Node _ _ state _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node dad _ _ _ _) = dad

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ _ depth) = depth

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ _ _ action _) = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ copilas _ _ _) = copilas

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = createRoot s

createRoot :: (ProblemState s a, Eq s) => s -> Node s a
createRoot s = Node Nothing (map (\x -> createCopilas x 1) (successors s)) s Nothing 0

createCopilas :: (ProblemState s a, Eq s) => (a, s) -> Int -> Node s a
createCopilas y@(a, s) depth = if (depth == 1) then Node (Just (createRoot (snd (reverseAction y)))) (map (\x -> createCopilas x (depth + 1)) (successors s)) s (Just a) depth 
                                else Node (Just (createCopilas (reverseAction y) (depth - 1))) (map (\x -> createCopilas x (depth + 1)) (successors s)) s (Just a) depth 

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

    S0 -> [S1, S3, S4]
    S1 -> [S0, S3]
    S2 -> [S3]
    S3 -> [S0, S1, S2]
    S4 -> [S0]

    bfs S0 -> [([S1, S3, S4], [S1, S3, S4]), ([] , [S3, S4]), ([S2], [S4, S2]), ([], [S2]), ([], [])]
    vizitati: [S0, S1, S4, S3, S2]
-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs (Node _ copii _ _ _ ) = (copii, copii) : (bfsRecursiv copii vizitati)
    where
        vizitati = copii

bfsRecursiv :: Eq s => [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfsRecursiv [] _ = []
bfsRecursiv (x:xs) vizitati = (copiiNevizitati, (xs ++ copiiNevizitati)) : (bfsRecursiv (xs ++ copiiNevizitati) vizitatiNoi)
    where
        copiiNoi = kids x
        copiiNevizitati = foldr (\y acc -> if elem y vizitati then acc else y : acc) [] copiiNoi
        vizitatiNoi = copiiNevizitati ++ vizitati



{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.

    bfs x -> [([S1, S2], [S1, S2]), ([S3], [S2, S3]), ...]
    bfs y -> [([S5, S6], [S5, S6]), ([S3], [S6, S3]), ...]
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS x y = comparari inceputX inceputY
    where
        inceputX = bfs x
        inceputY = bfs y

comparari :: Eq s => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
comparari (a:as) (b:bs) 
    | null comparare1 == False = head $ head comparare1
    | null comparare2 == False = head $ head comparare2
    | otherwise = comparari as bs 
    where
        inceputA = fst a
        sfarsitA = snd a
        inceputB = fst b
        sfarsitB = snd b
        comparare1 = foldr (\x acc -> if null (foldr (\y aacc -> if x == y then (x, y) : aacc else aacc) [] sfarsitB) == True then acc else (foldr (\y aacc -> if x == y then (x, y) : aacc else aacc) [] sfarsitB) : acc) [] inceputA
        comparare2 = foldr (\x acc -> if null (foldr (\y aacc -> if x == y then (x, y) : aacc else aacc) [] sfarsitA) == True then acc else (foldr (\y aacc -> if x == y then (x, y) : aacc else aacc) [] sfarsitA) : acc) [] inceputB



{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

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

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
