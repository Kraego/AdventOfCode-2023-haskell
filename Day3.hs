import Data.Char (isDigit)
import Data.Maybe(isJust, isNothing, fromJust)
import Control.Arrow (Arrow(first))
import Distribution.Simple.Utils (xargs)
import Distribution.Parsec (Parsec(parsec))
import Data.Time.Format.ISO8601 (yearFormat)

data GearNumber = GearNumber{
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

getNextGearNumber :: [Char] -> Int -> Maybe GearNumber -> Maybe GearNumber
getNextGearNumber [] _ _ = Nothing
getNextGearNumber xs row previous
    | isNothing startIndex = Nothing
    | isNothing previous = Just currentGearNumber
    | otherwise = Just (GearNumber (val currentGearNumber) (startIdx currentGearNumber + offsetPrevious) (endIdx currentGearNumber + offsetPrevious) row)
    where
        startIndex = getFirstDigitIdx $ zip [0..] xs
        number = getNumber (drop (fromJust startIndex) xs)
        currentGearNumber = GearNumber (read number) (fromJust startIndex) (fromJust startIndex + length number) row
        offsetPrevious = endIdx $ fromJust previous

getGearNumbers :: [Char] -> Int -> Maybe GearNumber -> [GearNumber]
getGearNumbers xs row previous
   | isNothing current = []
   | otherwise = fromJust current:getGearNumbers (drop ((endIdx . fromJust) current) xs) row current
    where
        current = getNextGearNumber xs row previous

readInMachineGears :: FilePath -> IO [GearNumber]
readInMachineGears fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getGearNumbers (snd x) (fst x) Nothing) fileLines
  return $ concat records

getNextSymbol :: [(Int, Char)] -> Int -> Int -> Maybe PlanSymbol
getNextSymbol [] _ _ = Nothing
getNextSymbol xs row offset
    | isJust firstSymbolIdx = Just (PlanSymbol (fromJust firstSymbolIdx + offset) row)
    | otherwise = Nothing
    where
        firstSymbolIdx = getFirstSymbolIdx xs

getPlanSymbols:: [(Int, Char)] -> Int -> Int -> [PlanSymbol]
getPlanSymbols xs row offset
   | isNothing current = []
   | otherwise = fromJust current:getPlanSymbols (drop (currentIdx + 1) xs) row (currentIdx - 1)
    where
        current = getNextSymbol xs row offset
        currentIdx = (idx . fromJust) current

readInPlanSymbols :: FilePath -> IO [PlanSymbol]
readInPlanSymbols fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getPlanSymbols (zip [0..] (snd x)) (fst x) 0) fileLines
  return $ concat records

countGears :: [GearNumber] -> [PlanSymbol] -> Int
countGears [] _ = 0
countGears (x:xs) ys = getAdjacentCount x ys + countGears xs ys

isAdjacent :: GearNumber -> PlanSymbol -> Bool
isAdjacent x y
    | (rowIdx y == row x) && abs (idx y - startIdx x) <= 1 = True --symbol left
    | (rowIdx y == row x) && abs (idx y - endIdx x) <= 1 = True -- symbol right
    | abs (rowIdx y - row x) == 1 && (abs (idx y - startIdx x) <= 2 || abs (idx y - endIdx x) <= 2) = True -- symbol below/above/diagonal
    | otherwise = False

getAdjacentCount :: GearNumber -> [PlanSymbol] -> Int
getAdjacentCount x ys
    | any (isAdjacent x) ys = val x
    | otherwise = 0

main :: IO ()
main = do
  gears <- readInMachineGears "./Input/Day3.txt"
  planSymbols <- readInPlanSymbols "./Input/Day3.txt"
  print gears
  -- print planSymbols
  let count = countGears gears planSymbols
  print count