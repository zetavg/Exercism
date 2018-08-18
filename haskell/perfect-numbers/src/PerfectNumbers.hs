module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | aliquotSum x == x = Just Perfect
  | aliquotSum x > x = Just Abundant
  | aliquotSum x < x = Just Deficient

aliquotSum :: Int -> Int
aliquotSum = sum.init.factors

factors :: Int -> [Int]
factors x = [y | y <- [1..x], x `mod` y == 0]
