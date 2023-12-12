import Data.Char (isDigit)
import Data.List(findIndex)
import Data.Maybe(isJust, isNothing, fromJust)

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

getFirstDigitIdx :: [(Int, Char)] -> Maybe Int
getFirstDigitIdx = findIndex (isDigit . snd)

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'

getNumber :: [Char] -> [Char]
getNumber = takeWhile isDigit

countDigits :: (Num a, Integral t) => t -> a
countDigits 0 = 0
countDigits n = 1 + countDigits (n `div` 10)

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

getPlanSymbols:: [(Int, Char)] -> Int -> [PlanSymbol]
getPlanSymbols xs row = map (\x -> PlanSymbol (fst x) row) $ filter (isSymbol . snd) xs

readInPlanSymbols :: FilePath -> IO [PlanSymbol]
readInPlanSymbols fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getPlanSymbols (zip [0..] (snd x)) (fst x)) fileLines
  return $ concat records

isAdjacent :: GearNumber -> PlanSymbol -> Bool
isAdjacent x y
    | abs (rowIdx y - row x) <= 1 && (idx y >= startIdx x && idx y <= endIdx x) = True --symbol below / above
    | abs (rowIdx y - row x) <= 1 && (abs (idx y - startIdx x) <= 1 || abs (idx y - endIdx x) <= 1) = True -- symbol left/right
    | otherwise = False

getAdjacentCount :: [PlanSymbol] -> GearNumber -> Int
getAdjacentCount xs y
    | any (isAdjacent y) xs = val y
    | otherwise = 0

countGears :: [GearNumber] -> [PlanSymbol] -> Int
countGears xs ys = sum $ map (getAdjacentCount ys) xs

isAdjacentToGear :: PlanSymbol -> GearNumber -> Bool
isAdjacentToGear y x = isAdjacent x y

countRatio :: PlanSymbol -> [GearNumber] -> Int
countRatio _ [] = 0
countRatio x ys
    | length matchings == 2 = product $ map val matchings
    | otherwise = 0
    where
        matchings = filter (isAdjacentToGear x) ys

calculateGearRatio :: [PlanSymbol] -> [GearNumber] -> Int
calculateGearRatio xs ys =  sum $ map (`countRatio` ys) xs

main :: IO ()
main = do
  gears <- readInMachineGears "./Input/Day03.txt"
  planSymbols <- readInPlanSymbols "./Input/Day03.txt"
--   mapM_ print gears
--   mapM_ print planSymbols
  let count = countGears gears planSymbols
  print count
  let gearRatio = calculateGearRatio planSymbols gears
  print gearRatio
