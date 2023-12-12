readLineNumber :: String -> Int
readLineNumber x = read $ concat (tail (words x))

readLineNumbers :: String -> [Int]
readLineNumbers x = map read $ tail $ words x

readInGame1 :: FilePath -> IO [(Int, Int)]
readInGame1 fp = do
    contents <- readFile fp
    let gameData = lines contents
        times = readLineNumbers $ head gameData
        distances = readLineNumbers $ last gameData
    return $ zip times distances

readInGame2 :: FilePath -> IO (Int, Int)
readInGame2 fp = do
    contents <- readFile fp
    let gameData = lines contents
        time = readLineNumber $ head gameData
        distance = readLineNumber $ last gameData
    return (time, distance)

distances :: Int -> [Int]
distances time = map (\x-> (time - x) * x) [0..time]

wins :: (Int,Int) -> Int
wins (time, distance) = length $ filter (>distance) $ distances time

main :: IO ()
main = do
  gameDataPart1 <- readInGame1 "./Input/Day06.txt"
  print $ product $ map wins gameDataPart1
  gameDataPart2 <- readInGame2 "./Input/Day06.txt"
  print $ wins gameDataPart2