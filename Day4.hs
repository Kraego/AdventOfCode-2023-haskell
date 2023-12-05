import Data.Char (isDigit)
import Data.Maybe(isJust, isNothing, fromJust)

data CardData = CardData{
    numbers :: [Int],
    winnings :: [Int]
    } deriving Show

getFirstIdxOfSymbol :: [Char] -> Char -> Maybe Int
getFirstIdxOfSymbol [] s = Nothing
getFirstIdxOfSymbol xs s
    | not (null symIdx) = Just (fst $ head symIdx)
    | otherwise = Nothing
    where
        symIdx = filter (\x-> snd x == s) (zip [0..] xs)

getNumbersPart :: [Char] -> [Char]
getNumbersPart [] = []
getNumbersPart xs
    | isNothing startIdx || isNothing endIdx = []
    | otherwise = take (fromJust endIdx - fromJust startIdx - 1) $ drop (fromJust startIdx + 1) xs
    where
        startIdx = getFirstIdxOfSymbol xs ':'
        endIdx = getFirstIdxOfSymbol xs '|'

getWinningsPart :: [Char] -> [Char]
getWinningsPart [] = []
getWinningsPart xs
        | isNothing startIdx = []
        | otherwise = drop (fromJust startIdx + 1) xs
    where
        startIdx = getFirstIdxOfSymbol xs '|'

getNumber :: [Char] -> [Char]
getNumber [] = []
getNumber (x:xs)
    | isDigit x = x:getNumber xs
    | otherwise = []

getNumberStr :: [Char] -> [[Char]]
getNumberStr xs
    | null xs = []
    | otherwise = number : getNumberStr (drop toDrop xs)
    where
        number = getNumber xs
        toDrop = length number + 1

getNumbers :: [Char] -> [Int]
getNumbers [] = []
getNumbers xs = map read $ filter (/= "") $ getNumberStr xs

getCardData :: [Char] -> CardData
getCardData xs = CardData numbersPart winningsPart
    where
        numbersPart = getNumbers $ getNumbersPart xs
        winningsPart = getNumbers $ getWinningsPart xs

readInCard :: FilePath -> IO [CardData]
readInCard fp = do
  contents <- readFile fp
  let cards = map getCardData $ lines contents
  return cards

countPointMatches :: [Int] -> [Int] -> Int
countPointMatches [] _ = 0
countPointMatches _ [] = 0
countPointMatches (x:xs) ys
    | x `elem` ys = 1 + countPointMatches xs ys
    | otherwise = countPointMatches xs ys

countCardPoints :: CardData -> Int
countCardPoints card
    | points == 0 = 0
    | otherwise = 2^(points - 1)
    where 
        points = countPointMatches (numbers card) (winnings card)

main :: IO ()
main = do
  cards <- readInCard "./Input/Day4.txt"
  -- print cards
  let points = sum (map countCardPoints cards)
  print points