module Main where
import Control.Applicative
import Data.List

-- The Smallest Free Number.
-- Given a list of integers without duplicates, find
-- the smallest natural number that is not in the list.

solve :: [Int] -> Int
solve xs = go 0 (length xs) xs
  where
    go lower higher [] = lower
    go lower higher xs | length left < middle - lower = go lower middle left
                       | otherwise = go middle higher right
      where
        (left, right) = partition (< middle) xs
        middle = (lower + higher + 1) `div` 2

main = do
    input <- (map read <$> words <$> getLine) :: IO [Int]
    print . solve . filter (>= 0) $ input