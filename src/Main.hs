{-

    Fox ang Geese game
    
-}

module Main where

import Data.Char

import Game
import Drawing
import Input
import File

main = do
        game initState
        return ()
        
{-
    Glowna petla gry
-}
game :: GameState -> IO ()
game state =    do
                    putStrLn ""
                    if isTerminal state then do
                                                putStrLn "Koniec gry!"
                                                putStr "Przegrała strona: "
                                                putStrLn $ show $ translateSide $ currentPlayer state
                    else do
                        if not (isGeeseMove state) then
                            do
                                drawGame state
                                printCommandPrompt
                                line <- getLine
                                executeCommand line state
                        else
                            do
                                putStrLn "Owce wykonuja ruch!"
                                game $ makeComputerGooseMove state
 
{-
    Metoda obslugi polecenia gracza Wilka
-} 
executeCommand :: String -> GameState -> IO ()
executeCommand line state = case (translateToCommand line) of
                                    UNKNOWN -> do
                                                putStrLn "Nieznana komenda lub bledna pozycja!"
                                                game state
                                    RESTART -> game initState
                                    SAVE -> do
                                                saveStateToFile state
                                                game state
                                    LOAD -> do
                                                loaded <- readStateFromFile
                                                case loaded of 
                                                    Nothing -> game state
                                                    Just state' ->  game state'
                                    EXIT -> return ()
                                    (MoveTo pos) -> do
                                                    if isMoveValid (getFoxPosition state) pos state then
                                                        game $ makeMove (getFoxPosition state) pos state
                                                    else do
                                                        putStrLn "Nieprawidlowy ruch!"
                                                        game state