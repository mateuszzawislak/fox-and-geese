module File (saveStateToFile, readStateFromFile) where

import Data.Char
import System.IO
import qualified Data.Map as Map
import System.Directory

import Game

{-
    Funkcja zapisu do pliku
-}

saveStateToFile :: GameState -> IO ()
saveStateToFile state = do putStr "Podaj nazwe pliku: "
                           filePath <- getLine
                           if(filePath == "") then
                                do putStrLn "Nie podano nazwy pliku!"
                                   return ()
                           else do
                                   handle <- openFile filePath WriteMode
                                   hPrint handle $ fromEnum (currentPlayer state)
                                   savePieceToFile handle $ Map.toList (board state)
                                   hClose handle

{-
    Zapis pojedynczego elementu mapy Board
-}

savePieceToFile :: Handle -> [(Position, Side)] -> IO ()
savePieceToFile _ [] = return ()
savePieceToFile handle (((x,y),side):xs) = do
                                            hPutStrLn handle $ ( show x ) ++ " "  ++ ( show y ) ++ " " ++ ( show (fromEnum side) )
                                            savePieceToFile handle xs
                                            return ()
 
{-
    Funkcja oczytu z pliku
-}

readStateFromFile :: IO (Maybe GameState)
readStateFromFile = do putStr "Podaj nazwe pliku: "
                       filePath <- getLine
                       fileExists <- doesFileExist filePath
                       if fileExists then do handle <- openFile filePath ReadMode
                                             line <- hGetLine handle
                                             board <- readPiecesFromFile handle
                                             hClose handle
                                             case board of
                                                Nothing -> do putStrLn "Bledny format pliku!"
                                                              return (Nothing)
                                                Just b -> return (Just (GameState (Map.fromList b) (toEnum (read line :: Int)) ))
                       else do putStrLn "Plik nie istnieje!"
                               return (Nothing)

{-
    Rekurencyjny odczyt elementow mapy Board z pliku
        Zwraca Nothing jezeli format pliku jest nieodpowiedni
-}

readPiecesFromFile :: Handle -> IO (Maybe [(Position, Side)])
readPiecesFromFile handle = do ineof <- hIsEOF handle
                               if ineof then return (Just [])
                               else do line <- hGetLine handle
                                       xs <- readPiecesFromFile handle
                                       case xs of
                                        Nothing -> return (Nothing)
                                        Just rest -> case me of
                                                        Nothing -> return (Nothing)
                                                        Just piece -> return (Just (piece:rest))
                                                        where me = readPieceFromString line;

{-
    Konwertuje String na element mapy Board
-}
readPieceFromString :: String -> (Maybe (Position, Side))
readPieceFromString line | 3 == (length list) = ( Just(((read (list !! 0) :: Int),(read (list !! 1) :: Int)), toEnum (read (list !! 2) :: Int)))
                         | otherwise = Nothing
                         where list = words  line