module Main where

import Nouns

import System.Environment
import System.Exit
import System.Random
import Data.Char
import Data.Maybe

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
main = do
    args <- getArgs
    name <- generateName $ getFirstLetter args
    adjective <- getRandomElement adjectives
    putStrLn ((capitalize adjective) ++ " " ++ (capitalize (berrify name)))
