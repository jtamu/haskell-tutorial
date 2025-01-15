{-# OPTIONS -Wall -Werror #-}

import Data.Map qualified as Map
import Data.Char

phoneBook :: Map.Map String String
phoneBook =
  Map.fromList
    [ ("betty", "555-2938"),
      ("bonnie", "452-2928")
    ]

doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe _ = "Not between 1 and 3"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Alice"
charName 'b' = "Bob"
charName _ = "Unknown"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string!"
firstLetter all'@(x : _) = "The first letter of " ++ all' ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight!"
  | bmi <= 25.0 = "You're normal!"
  | bmi <= 30.0 = "You're overweight!"
  | otherwise = "You're obese!"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
  | bmi <= skinny = "You're underweight!"
  | bmi <= normal = "You're normal!"
  | bmi <= fat = "You're overweight!"
  | otherwise = "You're obese!"
  where
    bmi = weight / height ** 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ** 2

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
