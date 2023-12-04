import Data.Char (isDigit)
import Data.Maybe(isJust, isNothing, fromJust)

data MotorValue = MotorValue{
    val :: Int,
    startIdx :: Int,
    endIdx :: Int,
    row :: Int
    } deriving Show

getFirstDigitIdx :: [(Int, Char)] -> Maybe Int
getFirstDigitIdx [] = Nothing
getFirstDigitIdx (x:xs)
    | isDigit $ snd x = Just $ fst x
    | otherwise = getFirstDigitIdx xs

getNumber :: [Char] -> [Char]
getNumber [] = []
getNumber (x:xs)
    | isDigit x = x:getNumber xs
    | otherwise = []

getNextMotorValue :: [Char] -> Int -> Maybe MotorValue -> Maybe MotorValue
getNextMotorValue [] _ _ = Nothing
getNextMotorValue xs row previous
    | isNothing startIndex = Nothing
    | isNothing previous = Just currentMotorValue
    | otherwise = Just (MotorValue (val currentMotorValue) (startIdx currentMotorValue + startIdx previousMotorValue) (endIdx currentMotorValue + startIdx previousMotorValue) row)
    where
        startIndex = getFirstDigitIdx $ zip [0..] xs
        number = getNumber (drop (fromJust startIndex) xs)
        currentMotorValue = MotorValue (read number) (fromJust startIndex) (fromJust startIndex + length number) row
        previousMotorValue = fromJust previous

getMotorValues :: [Char] -> Int -> Maybe MotorValue -> [MotorValue]
getMotorValues xs row previous 
   | isNothing current = []
   | otherwise = fromJust current:getMotorValues (drop (endIdx (fromJust current)) xs) row current
    where
        current = getNextMotorValue xs row previous
     
