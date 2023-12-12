import Data.List (elemIndex, partition)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, elemAt, toList)

data Hand =  Hand {cards :: String, strength :: Int,  bet :: Int} deriving Show

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
    where (lesser, greater) = partition (< p) xs

hasLessEqKicker :: String -> String -> Bool
hasLessEqKicker [] _ = True -- equal
hasLessEqKicker h1 h2
    | h1Kicker == h2Kicker = hasLessEqKicker (tail h1) (tail h2)
    | otherwise = h1Kicker < h2Kicker
    where
        h1Kicker = fromJust $ cardValue $ head h1
        h2Kicker = fromJust $ cardValue $ head h2

instance Eq Hand where
    (Hand h1 s1 _) == (Hand h2 s2 _) = s1 == s2 && h1 == h2

instance Ord Hand where
    (<=) (Hand h1 s1 _) (Hand h2 s2 _)
        | s1 < s2 = True
        | s1 == s2 && h1 `hasLessEqKicker` h2 = True
        | otherwise = False

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
handFromString xs = Hand {cards = hand, strength = handStrength hand, bet = read $ last handItems}
    where
        handItems = words xs
        hand = head handItems

readInHands :: FilePath -> IO [Hand]
readInHands fp = do
    contents <- readFile fp
    let handData = lines contents
    return $ map handFromString handData

calculateGames :: [(Hand, Int)] -> Int
calculateGames hands = sum $ map (\x -> bet (fst x) * snd x) hands


main :: IO ()
main = do
  handData1 <- readInHands "./Input/Day7.txt"
  let sortedHands = quicksort handData1
      rankedHands = zip sortedHands [1..]
  print $ calculateGames rankedHands
