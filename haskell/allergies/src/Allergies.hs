module Allergies (Allergen(..), allergies, isAllergicTo) where

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

listOfAllAllergies =
  [ Eggs
  , Peanuts
  , Shellfish
  , Strawberries
  , Tomatoes
  , Chocolate
  , Pollen
  , Cats ]

bits :: Int -> [Bool]
bits number
  | number <= 0 = []
  | mod == 1 = True : bits div
  | mod == 0 = False : bits div
  where (div, mod) = number `divMod` 2

maskBy :: [a] -> [Bool] -> [a]
[] `maskBy` _ = []
_ `maskBy` [] = []
(x:xs) `maskBy` (b:bs) = if b then x:maskedTail else maskedTail
  where maskedTail = xs `maskBy` bs

allergiesOfBits :: [Bool] -> [Allergen]
allergiesOfBits = (listOfAllAllergies `maskBy`)

allergies :: Int -> [Allergen]
allergies = allergiesOfBits . bits

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
