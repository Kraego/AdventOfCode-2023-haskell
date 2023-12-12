import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)

data CubeRecord = CubeRecord{
    gameId :: Int,
    red :: Int,
    green :: Int,
    blue :: Int
    } deriving Show

getToken :: String -> String
getToken = takeWhile (\x -> x /= ',' && x /= ':' && x /= ';')

tokenize :: String -> [String]
tokenize xs 
    | null xs = []
    | otherwise = token : tokenize (drop toDrop xs)
    where
        token = getToken xs
        toDrop = length token + 1

getGameId :: [Char] -> Int
getGameId xs = read $ last $ splitOn "Game " xs

getNumber :: String -> String
getNumber = filter isDigit

getTokensForKey :: [String] -> String -> [String]
getTokensForKey tokens key = filter (key `isInfixOf`) tokens

getMax :: [String] -> Int
getMax xs = maximum $ map (read . getNumber) xs

extractFromLine :: String -> CubeRecord
extractFromLine xs = CubeRecord gameId red green blue
    where
        tokens = tokenize xs
        gameId = getGameId $ head tokens
        red = getMax $ getTokensForKey tokens "red"
        green = getMax $ getTokensForKey tokens "green"
        blue = getMax $ getTokensForKey tokens "blue"

idSum :: [CubeRecord] -> Int
idSum xs = sum (map gameId xs)

readInRecords :: FilePath -> IO [CubeRecord]
readInRecords fp = do
  contents <- readFile fp
  let records = map extractFromLine $ lines contents
  return records

powerRecord :: CubeRecord -> Int
powerRecord record = red record * green record * blue record

powerOfCubes :: [CubeRecord] -> Int
powerOfCubes xs = sum (map powerRecord xs)

main :: IO ()
main = do
  records <- readInRecords "./Input/Day02.txt"
  let matching = filter (\x -> red x <= 12 && green x <= 13 && blue x <= 14) records
  print $ idSum matching
  print $ powerOfCubes records