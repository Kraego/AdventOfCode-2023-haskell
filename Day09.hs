import Data.Char (isDigit)
import Control.Monad (forM_)

readNumber :: String -> Int
readNumber (x:xs)
    | x == '-' = negate (readNumber xs)
    | otherwise = read (x:takeWhile isDigit xs)

toNumbers :: String -> [Int]
toNumbers x = map readNumber $ words x

predictNextHistory :: [Int] -> [Int]
predictNextHistory (x:xs)
    | null xs = []
    | otherwise = head xs - x : predictNextHistory xs

predictHistory :: [Int] -> [[Int]]
predictHistory [] = []
predictHistory xs
    | all (==0) xs = [xs]
    | otherwise = xs : predictHistory (predictNextHistory xs)

main :: IO ()
main = do
    input <- readFile "./Input/Day09.txt"
    let histories = map toNumbers (lines input)
        predictedHistories = map predictHistory histories
        result1 = sum $ map (foldr ((+) . last) 0) predictedHistories
        result2 = sum $ map (foldr ((-) . head) 0) predictedHistories
    print result1
    print result2
