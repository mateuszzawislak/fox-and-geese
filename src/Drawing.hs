module Drawing (drawGame, translateSide) where

import qualified Data.Map as Map
import Game

{-
    Rysuje plansze gry w zadanym stanie na ekran
-}

drawGame :: GameState -> IO ()
drawGame gameState = do
                putStrLn $ "   " ++ ( concat ( map (\n -> (show n) ++ " ") [0..7] ) )
                printLine 0 gameState
                return ()

{-
    Rysuje n-tą linię planszy w zadanym stanie gry
-}
               
printLine :: Int -> GameState -> IO ()
printLine n state = do
                    putStr $ " " ++ (show n) ++ " "
                    printLine' ( map (\(x, y) -> (x,y)) [(x, y) | x <- [0..7], y <- [0..7], x > (n-1), x < (n+1)] ) state
                    if n == 7 then return ()
                    else printLine (n+1) state
                    return ()

{-
    Rysuje pola w zadanej lini planszy
-}                    

printLine' :: [Position] -> GameState -> IO ()
printLine' [] _ = do
                putStrLn ""
                return ()
printLine' (x:xs) state = do
                            putChar $ getSign x state
                            putChar ' '
                            printLine' xs state
                            return ()

{-
    Zwraca znak jakim ma być reprezentowane zadane pole w zadanym stanie gry
-}     
                       
getSign :: Position -> GameState -> Char
getSign pos state | (validPosition pos) = getSign' $ getPiece pos state
                  | otherwise = '_'

-- pomocnicza
getSign' :: (Maybe Side) -> Char
getSign' (Just Fox) = 'W'
getSign' (Just Goose) = 'O'
getSign' Nothing = 'x'

{-
    Funkcja tlumaczaca nazwe gracza na jezyk polski
-}

translateSide :: Side -> String
translateSide Fox = "Wilk"
translateSide Goose = "Owce"