import Data.Char (isDigit)
import Data.Maybe(isJust, isNothing, fromJust)
import Control.Arrow (Arrow(first))

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

countDigits :: (Num a, Integral t) => t -> a
countDigits 0 = 0
countDigits n = 1 + countDigits (div n 10)

getNextGearNumber :: [Char] -> Int -> Maybe GearNumber -> Maybe GearNumber
getNextGearNumber [] _ _ = Nothing
getNextGearNumber xs row previous
    | isNothing startIndex = Nothing
    | isNothing previous = Just relativeGearNumber
    | otherwise = Just (GearNumber (val relativeGearNumber) (startIdx relativeGearNumber + offsetPrevious + 1) (endIdx relativeGearNumber + offsetPrevious + 1) row)
    where
        startIndex = getFirstDigitIdx $ zip [0..] xs
        number = getNumber (drop (fromJust startIndex) xs)
        relativeGearNumber = GearNumber (read number) (fromJust startIndex) (fromJust startIndex + length number - 1) row
        offsetPrevious = endIdx $ fromJust previous

getGearNumbers :: [Char] -> Int -> Maybe GearNumber -> [GearNumber]
getGearNumbers xs row previous
   | isNothing current = []
   | otherwise = fromJust current:getGearNumbers (drop (startIndex + currentLen) xs) row current
    where
        current = getNextGearNumber xs row previous
        currentLen = (countDigits . val . fromJust) current
        startIndex = fromJust (getFirstDigitIdx $ zip [0..] xs)

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

getPlanSymbols:: [(Int, Char)] -> Int -> [PlanSymbol]
getPlanSymbols xs row 
   | isNothing current = []
   | otherwise = fromJust current:getPlanSymbols (drop (firstSymbolIdx + 1) xs) row 
    where
        current = getNextSymbol xs row 0
        currentIdx = (idx . fromJust) current
        firstSymbolIdx = fromJust $ getFirstSymbolIdx xs

readInPlanSymbols :: FilePath -> IO [PlanSymbol]
readInPlanSymbols fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getPlanSymbols (zip [0..] (snd x)) (fst x)) fileLines
  return $ concat records

countGears :: [GearNumber] -> [PlanSymbol] -> Int
countGears [] _ = 0
countGears (x:xs) ys = getAdjacentCount x ys + countGears xs ys

isAdjacent :: GearNumber -> PlanSymbol -> Bool
isAdjacent x y
    | abs (rowIdx y - row x) <= 1 && (idx y >= startIdx x && idx y <= endIdx x) = True --symbol below / above
    | abs (rowIdx y - row x) <= 1 && (abs (idx y - startIdx x) <= 1 || abs (idx y - endIdx x) <= 1) = True -- symbol left/right/diagonal
    | otherwise = False

getAdjacentCount :: GearNumber -> [PlanSymbol] -> Int
getAdjacentCount x ys
    | any (isAdjacent x) ys = val x
    | otherwise = 0

main :: IO ()
main = do
  gears <- readInMachineGears "./Input/Day3.txt"
  planSymbols <- readInPlanSymbols "./Input/Day3.txt"
-- print gears
-- print planSymbols
  let count = countGears gears planSymbols
  print count