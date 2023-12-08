import Data.List.Split (splitOn, chunksOf)
import Data.Array ( Ix(inRange) )

-- DestStart SourceStart rangeLength
type Range = (Int, Int, Int)

dest :: (a, b, c) -> a  
dest (x, _, _) = x  
  
srcRange :: (a, b, c) -> b  
srcRange (_, y, _) = y  
  
rangeLen :: (a, b, c) -> c  
rangeLen (_, _, z) = z  

getSeeds :: [[String]] -> [Int]
getSeeds xs = map read $ drop 1 $ words $ head $ head xs

getRanges :: [[String]] -> [[Range]]
getRanges = map (map ((\[x, y, z] -> (x, y, z)) . map read . words) . drop 1) . drop 1

categories :: String -> [String]
categories = splitOn "\n\n"

main :: IO ()
main = do
    inputs <- categories <$> readFile "./Input/Day5.txt"
    let input = map lines inputs
        ranges = getRanges input
        seeds = getSeeds input
    -- print ranges
    print $ minimum $ map (getLocation ranges) seeds 


getLocation :: [[Range]] -> Int -> Int
getLocation xs current = foldl (flip adaptCurrent) current xs

adaptCurrent :: [Range] -> Int -> Int
adaptCurrent [] current = current
adaptCurrent (x:xs) current
    | inRange (srcRange x, srcRange x + rangeLen x) current = dest x + current - srcRange x
    | otherwise = adaptCurrent xs current
