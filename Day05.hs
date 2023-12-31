import Data.List.Split (splitOn, chunksOf)
import Data.Array ( Ix(inRange, range) )

type Range = (Int, Int, Int)

dest :: (a, b, c) -> a
dest (x, _, _) = x

srcRange :: (a, b, c) -> b
srcRange (_, x, _) = x

rangeLen :: (a, b, c) -> c
rangeLen (_, _, x) = x

getSeeds :: [[String]] -> [Int]
getSeeds xs = map read $ drop 1 $ words $ head $ head xs

getRanges :: [[String]] -> [[Range]]
getRanges = map (map ((\[x, y, z] -> (x, y, z)) . map read . words) . drop 1) . drop 1

categories :: String -> [String]
categories = splitOn "\n\n"

main :: IO ()
main = do
    inputs <- categories <$> readFile "./Input/Day05.txt"
    let input = map lines inputs
        ranges = getRanges input
        seeds = getSeeds input
        -- results in many seeds :(
        expandedSeeds = concatMap (\(dest : srcRange : _) -> [dest .. (dest - 1 + srcRange)]) $ chunksOf 2 seeds
    -- mapM_ print ranges
    print $ minimum $ map (`getMappingResult` ranges) seeds
    print $ minimum $ map (`getMappingResult` ranges) expandedSeeds

getMappingResult :: Int -> [[Range]] -> Int
getMappingResult = foldl adaptCurrent

adaptCurrent :: Int -> [Range] -> Int
adaptCurrent current [] = current
adaptCurrent current (x:xs)
    | inRange (srcRange x, srcRange x + rangeLen x) current = dest x + current - srcRange x
    | otherwise = adaptCurrent current xs
