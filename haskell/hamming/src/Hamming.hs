module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x:xs) [] = Nothing
distance [] (y:ys) = Nothing
distance (x:xs) (y:ys) = case distance xs ys of Just d -> Just (if x == y then d else 1 + d)
                                                Nothing -> Nothing
