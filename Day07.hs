import Data.List (elemIndex, partition)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, elemAt, toList, member)

data Hand =  Hand {hand :: String, strength :: Int,  bet :: Int} deriving Show

updateStrength :: Hand -> Int -> Hand
updateStrength x strength = Hand {hand = hand x, strength = strength, bet = bet x}

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
cardValue x = elemIndex x "J23456789TQKA"

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
handFromString xs = Hand {hand = currentHand, strength = handStrength currentHand, bet = read $ last handItems}
    where
        handItems = words xs
        currentHand = head handItems

readInHands :: FilePath -> IO [Hand]
readInHands fp = do
    contents <- readFile fp
    let handData = lines contents
    return $ map handFromString handData

readInHandsWithJokers :: FilePath -> IO [Hand]
readInHandsWithJokers fp = do
    contents <- readFile fp
    let handData = lines contents
    return $ map (optimizeHand . handFromString) handData

optimizeHand :: Hand -> Hand
optimizeHand currHand
    | member 'J' cards && strength currHand < 6 = boostHand currHand $ strength currHand
    | otherwise = currHand
    where cards = fromList $ hand currHand

boostHand :: Hand -> Int -> Hand
boostHand currHand strength 
    | strength == 5 = updateStrength currHand 6                     -- four of Kind to five of kind
    | strength == 4 = updateStrength currHand 6                     -- full house to five of kind 
    | strength == 3 = updateStrength currHand 5                     -- three of a kind to four of a kind
    | strength == 2 && (jokers == 2) = updateStrength currHand 5    -- two pairs to four of a kind
    | strength == 2 = updateStrength currHand 4                     -- two pairs to full house
    | strength == 1 = updateStrength currHand 3                     -- one pair to three of a kind
    | strength == 0 = updateStrength currHand 1                     -- nothing to one pair
    where jokers = length $ filter (=='J') $ hand currHand

calculateGames :: [(Hand, Int)] -> Int
calculateGames hands = sum $ map (\x -> bet (fst x) * snd x) hands


main :: IO ()
main = do
  handData1 <- readInHands "./Input/Day07.txt"
  handData2 <- readInHandsWithJokers "./Input/Day07.txt"

  let sortedHands1 = quicksort handData1
      rankedHands1 = zip sortedHands1 [1..]
      sortedHands2 = quicksort handData2
      rankedHands2 = zip sortedHands2 [1..]

  -- mapM_ print rankedHands1
  print $ calculateGames rankedHands1
  -- mapM_ print rankedHands2
  print $ calculateGames rankedHands2
