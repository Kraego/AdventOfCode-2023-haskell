import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

sections :: String -> [String]
sections = splitOn "\n\n"

type Position = (String, String)

leftPos :: (a, b) -> a
leftPos (x, _) = x

rightPos :: (a, b) -> b
rightPos (_, y) = y

toPosition :: String -> (String, Position)
toPosition xs = (idxStr, (leftStr, rightStr))
    where
        splits = splitOn " = " xs
        idxStr = head splits
        leftStr = takeWhile (/=',') . drop 1 . last $ splits
        rightStr = init . drop 2 . dropWhile (/=',') . last $ splits

getPosition :: Map.Map String (String, String) -> String -> (String, String)
getPosition positions posId = fromMaybe ("---", "---") (Map.lookup posId positions)

countSteps :: Map.Map String Position -> [Char] -> String -> Int -> Int
countSteps positions (d:ds) currentId steps
    | currentId == "ZZZ" = steps
    | otherwise = countSteps positions ds newPositionIdx (steps + 1)
    where
        currentPos = getPosition positions currentId
        newPositionIdx = if d == 'L' then leftPos currentPos else rightPos currentPos

toPositionIdx :: String -> String
toPositionIdx = head . words

main :: IO ()
main = do
    inputs <- sections <$> readFile "./Input/Day08.txt"
    let input = map lines inputs
        directions = cycle $ head $ head input
        positions = Map.fromList $ map toPosition $ last input
        lastPosition = toPosition $ last $ last input
        startPosition = toPosition $ head $ last input
    -- print directions
    -- mapM_ print positions
    print $ countSteps positions directions "AAA" 0

