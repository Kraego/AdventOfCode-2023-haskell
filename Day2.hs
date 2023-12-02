import Data.Char (isDigit, digitToInt)
import Control.Monad 

data CubeRecord = CubeRecord{ 
    gameId :: Int,
    red :: Int, 
    green :: Int, 
    blue :: Int 
    } deriving Show

getToken :: [Char] -> [Char]
getToken (x:xs)
    | null xs = [x]
    | x == ',' = []
    | x == ';' = []
    | x == ':' = []
    | otherwise = [x] ++ getToken xs

tokenize :: [Char] -> [[Char]]
tokenize xs
    | null xs = []
    | otherwise = [token] ++ (tokenize $ drop toDrop xs)
    where
        token = getToken xs 
        toDrop = (length token) + 1

getGameId :: [Char] -> Int
getGameId xs = read $ drop keyLen xs
    where 
        key = "Game "
        keyLen = length key

getNumber :: [Char] -> [Char]
getNumber [] = []
getNumber (x:xs) 
    | isDigit x = [x] ++ getNumber xs
    | otherwise = getNumber xs

contains :: [Char] -> [Char] -> Bool 
contains xs ys
    | null xs = False    
    | ys == take (length ys) xs = True 
    | otherwise = contains (tail xs) ys

getTokensForKey :: [[Char]] -> [Char] -> [[Char]]
getTokensForKey tokens key = filter (\x -> contains x key) tokens

getSum :: [[Char]] -> Int
getSum [] = 0
getSum xs = read (getNumber(head xs)) + (getSum $ tail xs)

getMax :: [[Char]] -> Int
getMax [x] = read $ getNumber x
getMax (x:xs)
    | getMax xs > current = getMax xs
    | otherwise = current
    where 
        current = read $ getNumber x

extractFromLine :: [Char] -> CubeRecord
extractFromLine xs = CubeRecord gameId red green blue
    where
        tokens = tokenize xs 
        gameId = getGameId $ head tokens
        red = getMax $ getTokensForKey tokens "red"
        green = getMax $ getTokensForKey tokens "green"
        blue = getMax $ getTokensForKey tokens "blue"

idSum :: [CubeRecord] -> Int
idSum [] = 0
idSum xs = gameId (head xs) + idSum (tail xs)

readInRecords :: FilePath -> IO [CubeRecord]
readInRecords fp = do
  contents <- readFile fp
  let records = map extractFromLine $ lines contents
  return records

main :: IO ()
main = do
  records <- readInRecords "./Input/Day2.txt"
  let matching = filter (\x -> red x <= 12 && green x <= 13 && blue x <= 14) records
  print $ idSum matching