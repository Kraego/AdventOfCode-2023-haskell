readInGame :: FilePath -> IO [(Int, Int)]
readInGame fp = do
    contents <- readFile fp
    let gameData = lines contents
        times = map read (tail (words (head gameData)))
        distances = map read (tail (words (last gameData)))
    return $ zip times distances

distances :: Int -> [Int]
distances time = map (\x-> (time - x) * x) [0..time]

wins :: (Int,Int) -> Int
wins (time, distance) = length $ filter (>distance) $ distances time

main :: IO ()
main = do
  gameData <- readInGame "./Input/Day6.txt"
  print $ product $ map wins gameData