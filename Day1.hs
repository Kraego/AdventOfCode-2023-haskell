import Data.Char (isDigit, digitToInt)
import Data.Text(unpack)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

firstDigit :: [Char] -> Char
firstDigit (x:xs)
    | isDigit x = x
    | otherwise = firstDigit xs

sumLine :: [Char] -> Int
sumLine xs = read([firstDigit xs] ++ [firstDigit (reverse xs)])

sumWholeText :: [[Char]] -> Int
sumWholeText [] = 0
sumWholeText (x:xs) = (sumLine x) + sumWholeText xs

-- doIt "Day1aInput.txt"
doIt :: FilePath -> IO (Int)
doIt fp = do
  contents <- readFile fp
  let fileLines = lines contents
  return (sumWholeText fileLines)