module Game where

import qualified Data.Map as Map

import AI

-- A piece in the game
data Side = Fox | Goose
            deriving (Eq, Show, Enum)

-- A space on the board, counting from the lower left.
type Position = (Int, Int)
        
type Board = Map.Map Position Side

data GameState = GameState { 
                             board :: Board,
                             currentPlayer :: Side
                           } deriving (Show)

-------------------------
-- General functions
-------------------------

{-
    Sprawdza czy zadane pole planszy warcabowej jest czarne
-}

validPosition :: Position -> Bool
validPosition (x,y) = ( (x+y) `mod` 2 ) == 1

{-
    Zwraca czarne pola planszy warcabowej
-}

validPositions :: [(Position)]
validPositions = filter validPosition
                        [(x, y) | x <- [0..7], y <- [0..7]]

{-
    Zwraca strone gry ktora zajmuje zadana pozycje na planszy w zadanym stanie gry
-}
                        
getPiece :: Position -> GameState -> (Maybe Side)
getPiece  pos = Map.lookup pos . board

{-
    Zwraca inicjalny stan planszy
-}

initBoard = Map.fromList . concat $
            [
                zip (filter (\(x, y) -> x == 0) validPositions) (repeat Goose),
                [((7, 0), Fox)]
            ]

{-
    Zwraca inicjalny stan gry
-}

initState = GameState initBoard Fox
   
{-
    Zwraca pozycje wilka na planszy w zadanym stanie gry
-}

getFoxPosition :: GameState -> Position
getFoxPosition state = getFirstPosition' (Map.toList (board state)) Fox

{-
    Zwraca pierwsza napotkana pozycje na planszy zadanej strony gry
-}

getFirstPosition' :: [(Position, Side)] -> Side -> Position
getFirstPosition' [] _ = error "brak w liscie!"
getFirstPosition' ((pos,s):xs) side | s == side = pos
                                    | otherwise = getFirstPosition' xs side
{-
    Zwraca wszystkie liste pozycji jakie moze zadana strona osiagnac z zadanej listy pozycji startowych
-}

getAllPositions :: GameState -> Side -> [Position]
getAllPositions state side = getAllPositions' (Map.toList (board state)) side

-- pomocnicza
getAllPositions' :: [(Position, Side)] -> Side -> [Position]
getAllPositions' [] _ = []
getAllPositions' ((pos,s):xs) side | s == side = [pos] ++ rest
                                   | otherwise = rest
                                   where rest = getAllPositions' xs side

{-
    Zwraca pozycje na planszy ktore mozna osiagnac z zadanej pozycji
-}

adjacentPositions :: Position -> Side  -> [Position]
adjacentPositions (px, py) side = filter onBoard ( map addOffset $ offsets )
    where onBoard = (`elem` validPositions)
          offsets = filter adjacent (getReachSize side)
          adjacent delta@(dx, dy) = (not (delta == (0,0))) && ((px + py) `mod` 2 == 1)
          addOffset (dx, dy) = (px + dx, py + dy)
          
getReachSize :: Side -> [Position]
getReachSize Fox = [(x, y) | x <- [-1..1], y <- [-1..1]]
getReachSize Goose = [(x, y) | x <- [0..1], y <- [-1..1]]

{-
    Mowi kto ma nastepny ruch w grze
-}

getNextSide :: Side -> Side
getNextSide Fox = Goose
getNextSide Goose = Fox

{-
    Zwraca niezajete pozycje na plaszy ktore mozna osiagnac z zadanej pozycji przez podanego gracza
-}

possibleMoves :: Position -> Side -> GameState -> [Position]
possibleMoves position side gamestate = filter (\newPos -> (getPiece newPos gamestate) == Nothing ) $ adjacentPositions position side

{-
    Sprawdza czy mozna wykonac dany ruch
-}

isMoveValid :: Position -> Position -> GameState -> Bool
isMoveValid position endPos state | ((getPiece position state) == (Just (currentPlayer state))) && (elem endPos (possibleMoves position (currentPlayer state) state)) && ((getPiece endPos state) == Nothing) = True
                                  | otherwise = False

-------------------------
-- AI functions
-------------------------

{-
    sprawdza czy stan jest terminalny
-}

isTerminal :: GameState -> Bool
isTerminal state | [] == ( possibleMoves foxPosition Fox state) = True
                 | x == 0 = True
                 | otherwise = False
                 where foxPosition@(x,y) = getFoxPosition state

                 
{-
    zaslepka dla heurystycznej funkcji kosztu
-}

getConstCost :: GameState -> Int
getConstCost state = 5

{-
    Heurystyczna funkcja kosztu, gdzie ocena planszy obliczana jest ze wzoru:
        ocena planszy = odleglosc wilka od gornej ramy - suma odleglosci owc od wilka
        jezeli owca jest na tym samym poziomie lub powyzej od wilka to dodatkowo ocena jest obnizana o 5
-}

getGeeseCost :: GameState -> Int
-- jezeli stan jest terminalny i wygrywaja owce to zwroc maxa
-- jezeli wygral wilk to zwroc wartosc minimalna
getGeeseCost state | row == 0 = -100
                   | isTerminal state = getGeeseTermialCost
                   | otherwise = (4 * (row - 0))
                                    - ( foldl (+) 0 ( map (\goosePos -> getHeuristicDistance foxPosition goosePos) geesePositions ) )
                                    - ( foldl (+) 0 ( map (\(x,y) -> if not (row > x) then 5
                                                                     else 0 ) geesePositions ) )
                   where foxPosition@(row,col) = getFoxPosition state
                         geesePositions = (getAllPositions state Goose)
{-
    Zwraca ocene planszy dla satnu termianalnego
-}
             
getGeeseTermialCost :: Int
getGeeseTermialCost = 100
 
{-
    Heurystyczna funkcja przyblizonegj odlegloszci dwoch pol na planszy
-}

getHeuristicDistance :: Position -> Position -> Int
getHeuristicDistance (x,y) (r,c) = abs (x - r) + abs (y - c)


{-
    Sprawdza czy teraz jest ruch gracza. Jako gracza rozumiemy komputer czyli owce.
-}

isGeeseMove :: GameState -> Bool
isGeeseMove state | side == Goose = True
                  | otherwise = False
                  where side = currentPlayer state
                   

{-
    wykonuje ruch i zwraca nowy stan gry
-}

makeMove :: Position -> Position -> GameState -> GameState
makeMove start end state | not (isMoveValid start end state) = error "Proba wykonania nieprawidlowego ruchu!"
                         | otherwise = GameState (Map.fromList (switchPosition (Map.toList (board state)) start end)) (getNextSide (currentPlayer state))

{-
    Na zadanej planszy przestawia pionek z jednego pola na drugie
-}

switchPosition :: [(Position,Side)] -> Position -> Position -> [(Position,Side)]
switchPosition [] _ _ = error "Nie znaleziono pozycji do zamiany!"
switchPosition (k@((x,y),side):xs) pos@(row,col) newPos | (x == row) && (y == col) = (newPos,side):xs
                                                        | otherwise = [k] ++ ( switchPosition xs pos newPos )

                                                    
{-
    zwraca liste mozliwych stanow do ktorych moze przejsc dany stan gry
-}

getNextStates :: GameState -> [GameState]
getNextStates state = concat $ map (\startPos -> ( map (\nextPos -> makeMove startPos nextPos state ) (possibleMoves startPos (currentPlayer state) state) ) ) (getCurrentSidePositions state)

{-
    Funkcja zwraca liste pozycji zajmowanych na pamie przez gracza bedacego obecnie przy prawie ruchu
-}

getCurrentSidePositions :: GameState -> [Position]
getCurrentSidePositions state = getAllPositions' (Map.toList (board state)) (currentPlayer state)


{-
    funkcja wykonania ruchu przez komputer
-}

makeComputerGooseMove :: GameState -> GameState
makeComputerGooseMove state = getNextState state getNextStates isTerminal getGeeseCost isGeeseMove