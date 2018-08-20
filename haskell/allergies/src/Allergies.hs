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

allergiesOfBits :: [Bool] -> [Allergen]
allergiesOfBits =
  map (\(Just a) -> a)
    . filter (/= Nothing)
    . map (\(a, b) -> if b then Just a else Nothing)
    . zip listOfAllAllergies

allergies :: Int -> [Allergen]
allergies = allergiesOfBits . bits

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
