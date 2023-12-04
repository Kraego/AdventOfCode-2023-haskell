import Data.Char (isDigit)
import Data.Maybe(isJust, isNothing, fromJust)
import Control.Arrow (Arrow(first))

data PartNumber = PartNumber{
    val :: Int,
    startIdx :: Int,
    endIdx :: Int,
    row :: Int
    } deriving Show

getFirstDigitIdx :: [(Int, Char)] -> Maybe Int
getFirstDigitIdx [] = Nothing
getFirstDigitIdx (x:xs)
    | isDigit $ snd x = Just $ fst x
    | otherwise = getFirstDigitIdx xs

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
    | otherwise = Just (PartNumber (val currentPartNumber) (startIdx currentPartNumber + endIdx previousPartNumber) (endIdx currentPartNumber + endIdx previousPartNumber) row)
    where
        startIndex = getFirstDigitIdx $ zip [0..] xs
        number = getNumber (drop (fromJust startIndex) xs)
        currentPartNumber = PartNumber (read number) (fromJust startIndex) (fromJust startIndex + length number) row
        previousPartNumber = fromJust previous

getPartNumbers :: [Char] -> Int -> Maybe PartNumber -> [PartNumber]
getPartNumbers xs row previous 
   | isNothing current = []
   | otherwise = fromJust current:getPartNumbers (drop (endIdx (fromJust current)) xs) row current
    where
        current = getNextPartNumber xs row previous

readInMachineParts :: FilePath -> IO [PartNumber]
readInMachineParts fp = do
  contents <- readFile fp
  let fileLines = zip [0..] $ lines contents
  let records = map (\x -> getPartNumbers (snd x) (fst x) Nothing) fileLines  
  return $ concat records

main :: IO ()
main = do
  parts <- readInMachineParts "./Input/Day3.txt"
  print parts
     
