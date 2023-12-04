import Data.Char (isDigit)
import Data.Maybe(isJust, isNothing, fromJust)
import Control.Arrow (Arrow(first))
import Distribution.Simple.Utils (xargs)
import Distribution.Parsec (Parsec(parsec))
import Data.Time.Format.ISO8601 (yearFormat)

data PartNumber = PartNumber{
    val :: Int,
    startIdx :: Int,
    endIdx :: Int,
    row :: Int
    } deriving Show

data PlanSymbol = PlanSymbol{
    idx :: Int,
    rowIdx :: Int
} deriving Show

getFirstMatch :: ((Int, Char) -> Bool) -> [(Int, Char)] -> Maybe Int
getFirstMatch  _ [] = Nothing
getFirstMatch match (x:xs)
    | match x = Just $ fst x
    | otherwise = getFirstMatch match xs

getFirstDigitIdx :: [(Int, Char)] -> Maybe Int
getFirstDigitIdx = getFirstMatch (isDigit . snd)

isSymbol :: Char -> Bool
isSymbol x
    | isDigit x = False
    | x == '.' = False
    | otherwise = True

getFirstSymbolIdx :: [(Int, Char)] -> Maybe Int
getFirstSymbolIdx = getFirstMatch (isSymbol . snd)

getNumber :: [Char] -> [Char]
getNumber [] = []
getNumber (x:xs)
    | isDigit x = x:getNumber xs
    | otherwise = []

getNextPartNumber :: [Char] -> Int -> Maybe PartNumber -> Maybe PartNumber
getNextPartNumber [] _ _ = Nothing
getNextPartNumber xs row previous
    | isNothing startIndex = Nothing
    | isNothing previous = Just currentPartNumber
    | otherwise = Just (PartNumber (val currentPartNumber) (startIdx currentPartNumber + offsetPrevious) (endIdx currentPartNumber + offsetPrevious) row)
    where
        startIndex = getFirstDigitIdx $ zip [0..] xs
        number = getNumber (drop (fromJust startIndex) xs)
        currentPartNumber = PartNumber (read number) (fromJust startIndex) (fromJust startIndex + length number) row
        offsetPrevious = endIdx $ fromJust previous

getPartNumbers :: [Char] -> Int -> Maybe PartNumber -> [PartNumber]
getPartNumbers xs row previous
   | isNothing current = []
   | otherwise = fromJust current:getPartNumbers (drop ((endIdx . fromJust) current) xs) row current
    where
        current = getNextPartNumber xs row previous

readInMachineParts :: FilePath -> IO [PartNumber]
readInMachineParts fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getPartNumbers (snd x) (fst x) Nothing) fileLines
  return $ concat records

getNextSymbol :: [(Int, Char)] -> Int -> Int -> Maybe PlanSymbol
getNextSymbol [] _ _ = Nothing
getNextSymbol xs row offset
    | isJust firstSymbolIdx = Just (PlanSymbol (fromJust firstSymbolIdx + offset) row)
    | otherwise = Nothing
    where
        firstSymbolIdx = getFirstSymbolIdx xs

getPartSymbols:: [(Int, Char)] -> Int -> Int -> [PlanSymbol]
getPartSymbols xs row offset
   | isNothing current = []
   | otherwise = fromJust current:getPartSymbols (drop (currentIdx + 1) xs) row (currentIdx - 1)
    where
        current = getNextSymbol xs row offset
        currentIdx = (idx . fromJust) current

readInPartSymbols :: FilePath -> IO [PlanSymbol]
readInPartSymbols fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getPartSymbols (zip [0..] (snd x)) (fst x) 0) fileLines
  return $ concat records

countParts :: [PartNumber] -> [PlanSymbol] -> Int
countParts [] _ = 0
countParts (x:xs) ys = getAdjacentCount x ys + countParts xs ys

isAdjacent :: PartNumber -> PlanSymbol -> Bool
isAdjacent x y
    | (rowIdx y == row x) && abs (idx y - startIdx x) <= 1 = True --symbol left
    | (rowIdx y == row x) && abs (idx y - endIdx x) <= 1 = True -- symbol right
    | abs (rowIdx y - row x) == 1 && (abs (idx y - startIdx x) <= 1 || abs (idx y - endIdx x) <= 1) = True -- symbol below/above/diagonal
    | otherwise = False

getAdjacentCount :: PartNumber -> [PlanSymbol] -> Int
getAdjacentCount x ys
    | any (isAdjacent x) ys = val x
    | otherwise = 0

main :: IO ()
main = do
  parts <- readInMachineParts "./Input/Day3.txt"
  partSymbols <- readInPartSymbols "./Input/Day3.txt"
  print parts
  -- print partSymbols
  let count = countParts parts partSymbols
  print count