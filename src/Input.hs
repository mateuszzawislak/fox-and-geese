module Input(translateToCommand, printCommandPrompt, Command (..)) where

import Data.Char
import Data.Maybe

import Game

data Command a = EXIT | SAVE | LOAD | RESTART | UNKNOWN | MoveTo a

{-
    Konwertuje napis na pozycje na planszy
-}

stringToPosition :: String -> Maybe Position
stringToPosition string | not (2 == (length string)) = Nothing
                        | not (isDigit (string !! 0) && isDigit (string !! 1)) = Nothing
                        | otherwise = Just (digitToInt (string !! 0), digitToInt (string !! 1))

{-
    Konwertuje napis na pozycje na "czarna" planszy
-}

stringToValidPosition :: String -> Maybe Position
stringToValidPosition string | isJust position && elem pos validPositions = Just pos
                             | otherwise = Nothing
                             where position = stringToPosition string
                                   pos = fromJust position
 
{-
    Sprawdza przekazany napis jest zadanym znakiem Char
-} 

isSign :: String -> Char -> Bool
isSign string char | not ( 1 == (length string)) = False
                   | (string !! 0) == char = True
                   | otherwise = False
                   
isExit :: String -> Bool
isExit string = isSign string 'E'

isSave :: String -> Bool
isSave string = isSign string 'S'

isLoad :: String -> Bool
isLoad string = isSign string 'L'

isRestart :: String -> Bool
isRestart string = isSign string 'R'

{-
    Tlumaczy napis na komende podana przez uzytkownika
-}

translateToCommand :: String -> (Command Position)
translateToCommand string | isExit string = EXIT
                          | isSave string = SAVE
                          | isLoad string = LOAD
                          | isRestart string = RESTART
                          | (isJust position) = MoveTo (fromJust position)
                          | otherwise = UNKNOWN
                          where position = stringToValidPosition string
                          
{-
    Metoda wypisujaca znak zachety i instrukcje do gry
-}

printCommandPrompt :: IO ()
printCommandPrompt = do
                        putStrLn ""
                        putStrLn "Dostepne komendy:"
                        putStrLn "   E - Wyjscie"
                        putStrLn "   R - Restart gry"
                        putStrLn "   S - Zapis stanu gry do pliku"
                        putStrLn "   L - Odczyt stanu gry z pliku"
                        putStrLn "   ABY WYKONAC RUCH... Wpisz nastepna pozycje wilka podana w postaci WierszKolumna (np. 21)"
                        putStrLn ""
                        putStr "Podaj komende> "