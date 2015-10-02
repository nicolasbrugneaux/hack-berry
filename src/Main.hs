{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nouns

import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Data.Maybe
import Data.Text (unpack)
import System.Environment
import System.Exit
import System.Random
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)

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

response :: [String] -> IO String
response args = do
    name <- generateName $ getFirstLetter args
    adjective <- getRandomElement adjectives
    return ((capitalize adjective) ++ " " ++ (capitalize (berrify name)))

application request respond = do
    let args = map unpack $ pathInfo request
    let _status = if args!!0 == "favicon.ico" then status404 else status200
    msg <- response args
    respond $ responseLBS _status [("Content-Type", "text/plain")] (pack msg)


main :: IO ()
main = run 5002 $ gzip def application
