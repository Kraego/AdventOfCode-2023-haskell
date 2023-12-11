import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, elemAt, toList)
import GHC.Utils.Binary (handleData)
import GHC.Parser.Lexer (xset)

-- actualHand, strength, bet
type Hand =  (String, Int, Int)

betFromHand :: Hand -> Int
betFromHand (_, _, bet) = bet

cardValue :: Char -> Maybe Int
cardValue x = elemIndex x "23456789TJQKA"

countOccurrence :: Char -> String -> Int
countOccurrence x xs = length $ filter (== x) xs

isFourOfKind :: String -> Set Char -> Bool
isFourOfKind hand cards = occurrenceHead == 1 || occurrenceHead == 4
    where occurrenceHead = countOccurrence (elemAt 0 cards) hand

isThreeOfKind :: String -> String -> Bool
isThreeOfKind hand [] = False
isThreeOfKind hand cards = (occurrenceHead == 3) || isThreeOfKind hand (tail cards)
    where occurrenceHead = countOccurrence (head cards) hand

handStrength :: String -> Int
handStrength hand
    | differentCards == 1 = 6 -- five of a kind
    | differentCards == 4 = 1 -- one pair
    | differentCards == 2 && isFourOfKind hand handSet = 5 --four of kind
    | differentCards == 2 = 4 -- full house
    | differentCards == 3 && isThreeOfKind hand (toList handSet) = 3 -- three of a kind
    | differentCards == 3 = 2 -- two pairs
    | otherwise = 0
    where
        handSet = fromList hand
        differentCards = length handSet

handFromString :: String -> Hand
handFromString xs = (hand, handStrength hand, read $ last handItems)
    where 
        handItems = words xs
        hand = head handItems

readInHands :: FilePath -> IO [Hand]
readInHands fp = do
    contents <- readFile fp
    let handData = lines contents
    return $ map handFromString handData

hasGtEqKicker :: String -> String -> Bool
hasGtEqKicker [] _ = True
hasGtEqKicker hand1 hand2 
    | hand1Head == hand2Head = hasGtEqKicker (tail hand1) (tail hand2)
    | otherwise = hand1Head > hand2Head
    where
        hand1Head = fromJust $ cardValue $ head hand1
        hand2Head = fromJust $ cardValue $ head hand2

isGtHand :: Hand -> Hand -> Bool
isGtHand (hand1, score1, _) (hand2, score2, _) 
    | score2 == score1 = hasGtEqKicker hand1 hand2
    | otherwise = score1 > score2 

hasLessKicker :: String -> String -> Bool
hasLessKicker [] _ = False -- not equal 
hasLessKicker hand1 hand2 = hasGtEqKicker hand2 hand1

isLessHand :: Hand -> Hand -> Bool
isLessHand (hand1, score1, _) (hand2, score2, _) 
    | score2 == score1 = hasLessKicker hand1 hand2
    | otherwise = score1 < score2 

quicksort :: [Hand] -> [Hand]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (isLessHand p) xs
        greater = filter (isGtHand p) xs

calculateGames :: [(Hand, Int)] -> Int
calculateGames hands = sum $ map (\x -> betFromHand (fst x) * snd x) hands 

main :: IO ()
main = do
  handData1 <- readInHands "./Input/Day7.txt"
  -- print handData1
  let sortedHands = quicksort handData1
      rankedHands = zip (reverse sortedHands) [1..]
  print $ calculateGames rankedHands
