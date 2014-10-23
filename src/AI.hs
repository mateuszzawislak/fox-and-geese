module AI (getNextState) where

import qualified Data.Map as Map

-- (cost,state)
type TreeNode a = (Int, a)

-- functions

{-
    Funckje pomocnicza do obsługi wezlow drzewa gry
-}

getNodeCost :: (TreeNode a) -> Int
getNodeCost (n,_) = n

getNodeValue :: (TreeNode a) -> a
getNodeValue (_,value) = value

{-
    Funkcja wykonujaca ruch - Sztuczna inteligencja
-}

getNextState :: t -> (t -> [t]) -> (t -> Bool) -> (t -> Int) -> (t -> Bool) -> t
getNextState state getNextStates isTerminal getCost isPlayerMove = getNodeValue $ cuttedMiniMax maxDepth state getNextStates isTerminal getCost isPlayerMove

{-
    Maksymalna glebokosc przeszukiwania w algorytmie obcietego mini-maks'a
-}

maxDepth :: Int
maxDepth = 6

{-
    Algorytm: Obciety mini-maks
-}

cuttedMiniMax :: Int -> t -> (t -> [t]) -> (t -> Bool) -> (t -> Int) -> (t -> Bool) -> (TreeNode t)
cuttedMiniMax 0 state _ _ getCost _ = ( (getCost state) , state )
cuttedMiniMax k state getNextStates isTerminal getCost isPlayerMove | isTerminal state = ( (getCost state) , state )
                                                                    | isPlayerMove state = childrenMax
                                                                    | otherwise = childrenMin
                                                                    where
                                                                        childrenValues = map (\childState -> (getNodeCost (cuttedMiniMax (k-1) childState getNextStates isTerminal getCost isPlayerMove), childState)) $ getNextStates state
                                                                        childrenMax = getMax childrenValues
                                                                        childrenMin = getMin childrenValues

{-
    Zwraca maksymalny wezel z przekazanej listy wezlow
-}                                                        
                
getMax :: [(TreeNode a)] -> (TreeNode a)
getMax [x] = x
getMax (curr@(x,state):xs)   | x > x' = curr
                             | otherwise = child
                             where child@(x',state') = getMax xs
 
{-
    Zwraca minimalny wezel z przekazanej listy wezlow
-}
 
getMin :: [(TreeNode a)] -> (TreeNode a)
getMin [x] = x
getMin (curr@(x,state):xs)   | x < x' = curr
                             | otherwise = child
                             where child@(x',state') = getMin xs
