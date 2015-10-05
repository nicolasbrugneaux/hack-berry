module Main where

import Nouns

import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Data.Maybe
import Data.Text (unpack)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import System.Environment
import System.Random

port_number :: Int
port_number = 5002

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const . drop 1) xs (drop n xs)

getFirstLetter :: [String] -> Maybe Char
getFirstLetter [] = Nothing
getFirstLetter (x:_) = Just (head x)

getRandomElement :: [String] -> IO String
getRandomElement xs = do
    index <- randomRIO (0, (length xs) - 1)
    return (xs !! index)

generateName :: Maybe Char -> IO String
generateName Nothing = getRandomElement prefix
generateName initial = getRandomElement $ prefixByLetter (fromJust initial)

berrify :: String -> String
berrify str
    | "berry" == lastN 5 str = str
    | otherwise              = str ++ "berry"

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

response :: [String] -> IO String
response args = do
    name <- generateName $ getFirstLetter args
    adjective <- getRandomElement adjectives
    return ((capitalize adjective) ++ " " ++ (capitalize (berrify name)))

application :: Application
application request respond = do
    let args = map unpack $ pathInfo request
    let isFavicon = ((args /= []) && (args!!0 == "favicon.ico"))
    let _status = if isFavicon then status404 else status200
    msg <- response args
    respond $ responseLBS _status [("Content-Type", "text/plain")] (pack (msg ++ "\n"))


main :: IO ()
main = do
    env <- getEnvironment
    let port = maybe port_number read $ lookup "PORT" env
    putStrLn ("Server started at http://127.0.0.1:" ++ (show port))
    run port $ gzip def application
