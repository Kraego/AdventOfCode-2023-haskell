import Data.Char(isDigit)
import Data.Maybe(isJust, isNothing, fromJust)
import Data.List(intersect)
import Data.List.Split (splitOn)


data CardData = CardData{
    numbers :: [Int],
    winnings :: [Int]
    } deriving Show

getNumbersPart :: [Char] -> [Char]
getNumbersPart xs = last $ splitOn ":" $ head $ splitOn "|" xs

getWinningsPart :: [Char] -> [Char]
getWinningsPart xs = last $ splitOn "|" xs

getNumbers :: [Char] -> [Int]
getNumbers xs = map read $ filter (/= "") $ words xs

getCardData :: [Char] -> CardData
getCardData xs = CardData ((getNumbers . getNumbersPart) xs) ((getNumbers . getWinningsPart) xs)

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
updateCards xs idx current matches = [if fst x > idx && fst x <= idx + matches then (fst x , snd x + current) else x | x <- xs] 

countCards :: [CardData] -> Int -> [(Int,Int)] -> Int
countCards [] _ cards = sum $ map snd cards
countCards (x:xs) cardIdx cards = countCards xs (cardIdx + 1) $ updateCards cards cardIdx currentCards points
    where
        points = countPointMatches (numbers x) (winnings x)
        currentCards = snd $ cards!!cardIdx

main :: IO ()
main = do
  cards <- readInCard "./Input/Day04.txt"
  -- mapM_ print cards
  let points = sum (map countCardPoints cards)
  print points
  let cardCount = countCards cards 0 $ zip [0..] $ map (const 1) cards
  print cardCount
