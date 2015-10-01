module Main where

import Nouns

import Control.Concurrent
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.Random
import System.IO
import Network

port_number = 5002

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

getFirstLetter :: [String] -> Maybe Char
getFirstLetter [] = Nothing
getFirstLetter (x:xs) = Just (head x)

getRandomElement :: [String] -> IO String
getRandomElement xs = do
    index <- randomRIO (0, (length xs) - 1)
    return (xs !! index)

generateName :: Maybe Char -> IO String
generateName Nothing = getRandomElement prefix
generateName initial = getRandomElement $ prefixByLetter (fromJust initial)

berrify :: String -> String
berrify str = case lastN 5 str of
    "berry" -> str
    otherwise -> str ++ "berry"

capitalize :: String -> String
capitalize (x:xs) = (toUpper x):xs

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber port_number
    let port = show port_number
    putStrLn ("Listening on port " ++ port ++ ": http://127.0.0.1:" ++ port)
    loop sock

loop :: Socket -> IO ()
loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
   where
       body h = do
           name <- generateName $ getFirstLetter []
           adjective <- getRandomElement adjectives
           let berry = ((capitalize adjective) ++ " " ++ (capitalize (berrify name)))
           hPutStr h $ msg berry
           hFlush h
           hClose h

msg :: String -> String
msg str = "HTTP/1.0 200 OK\r\nContent-Length: " ++ len ++ "\r\n\r\n" ++ str ++ "\r\n"
    where
        len = show $ length str
