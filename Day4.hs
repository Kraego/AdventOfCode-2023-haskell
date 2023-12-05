import Data.Char(isDigit)
import Data.Maybe(isJust, isNothing, fromJust)
import Data.List(intersect)

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
countPointMatches xs ys = length $ intersect xs ys

countCardPoints :: CardData -> Int
countCardPoints card
    | points == 0 = 0
    | otherwise = 2^(points - 1)
    where
        points = countPointMatches (numbers card) (winnings card)

updateCards :: [(Int, Int)] -> Int -> Int -> Int -> [(Int, Int)]
updateCards [] _ _ _ = []
updateCards (x:xs) idx current matches
    | fst x > idx && fst x <= idx + matches = (fst x, snd x + current):updateCards xs idx current matches
    | otherwise = x:updateCards xs idx current matches

countCards :: [CardData] -> Int -> [(Int,Int)] -> Int
countCards [] _ cards = sum $ map snd cards
countCards (x:xs) cardIdx cards = countCards xs (cardIdx + 1) $ updateCards cards cardIdx currentCards points
    where 
        points = countPointMatches (numbers x) (winnings x)
        currentCards = snd $ cards!!cardIdx

main :: IO ()
main = do
  cards <- readInCard "./Input/Day4.txt"
  -- print cards
  let points = sum (map countCardPoints cards)
  print points
  let cardCount = countCards cards 0 $ zip [0..] $ map (const 1) cards
  print cardCount
