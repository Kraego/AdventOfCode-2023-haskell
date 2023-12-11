import Data.Char (isDigit)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Maybe (fromJust)
import Data.List (find)

-- address overlapping
replaceNumStrings :: [Char] -> [Char]
replaceNumStrings xs
    | null xs = xs
    | take 3 xs == "one"   = "o1e" ++ replaceNumStrings (drop 3 xs)
    | take 3 xs == "two"   = "t2o" ++ replaceNumStrings (drop 3 xs)
    | take 5 xs == "three" = "t3e" ++ replaceNumStrings (drop 3 xs)
    | take 4 xs == "four"  = "f4r" ++ replaceNumStrings (drop 3 xs)
    | take 4 xs == "five"  = "f5e" ++ replaceNumStrings (drop 3 xs)
    | take 3 xs == "six"   = "s6x" ++ replaceNumStrings (drop 3 xs)
    | take 5 xs == "seven" = "s7n" ++ replaceNumStrings (drop 3 xs)
    | take 5 xs == "eight" = "e8t" ++ replaceNumStrings (drop 3 xs)
    | take 4 xs == "nine"  = "n9e" ++ replaceNumStrings (drop 3 xs)
    | otherwise = head xs : replaceNumStrings (tail xs)

firstDigit :: [Char] -> Char
firstDigit = fromJust. find isDigit -- boldly assume that there is a digit

sumLine :: [Char] -> Int
sumLine xs = read $ firstDigit xs : [firstDigit (reverse xs)]

sumWholeText :: [[Char]] -> Int
sumWholeText xs = sum (map sumLine xs)

doIt :: FilePath -> IO Int
doIt fp = do
  contents <- readFile fp
  let fileLines = map replaceNumStrings $ lines contents
  -- debug prints
  -- forM_ fileLines $ \l -> print l
  return $ sumWholeText fileLines

main :: IO ()
main = do
  result <- doIt "./Input/Day1.txt"
  print result