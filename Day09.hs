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
    where prediction = predictHistory xs

extrapolate :: [[Int]] -> Int
extrapolate [] = 0
extrapolate xs = last appendices
     where
         reversed = reverse xs
         appendices = calculateAppendices (map last reversed) 0

calculateAppendices :: [Int] -> Int -> [Int]
calculateAppendices (x:xs) prev
    | null xs = []
    | x == 0 = 0 : calculateAppendices xs current
    | otherwise = current : calculateAppendices xs current
    where current = prev + head xs

main :: IO ()
main = do
    input <- readFile "./Input/Day09.txt"
    let histories = map toNumbers (lines input)
        result1 = sum $ map (extrapolate . predictHistory) histories
    -- print $ map predictHistory histories
    -- print $ map (extrapolate . predictHistory) histories
    print result1
