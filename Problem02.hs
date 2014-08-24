module Main where
import Control.Applicative
import Data.List
import Test.QuickCheck

-- A surpassing problem.
-- Given a list find maximum surpasser count, where  
-- surpasser count for an element x[i] is number of
-- elements x[j] s.t. x[i] < x[j] and i < j.

-- Specification from the book.
msc :: Ord a => [a] -> Int
msc [] = 0
msc xs = maximum [scount z zs | z : zs <- tails xs]
  where
    scount x xs = length (filter (x <) xs)
    tails [] = []
    tails (x : xs) = (x : xs) : tails xs

-- O(n*logn) solution.
solve :: Ord a => [a] -> Int
solve [] = 0 
solve xs = fst . maximum . go . zip (repeat 0) $ xs
  where
    go :: Ord a => [(Int, a)] -> [(Int, a)]
    go [x] = [x]
    go xs = merge (length right) (go left) (go right)
      where
        (left, right) = splitAt (length xs `div` 2 ) xs
        merge n [] ys = ys
        merge n xs [] = xs
        merge n (a@((nx, x) : xs)) (b@((ny, y) : ys))
            | x < y      = (nx + n, x) : merge n xs b 
            | otherwise  = (ny, y) : merge (n - 1) a ys

prop_spec = verboseCheck (\xs -> solve (xs::[Int]) == msc xs)

main = do
    input <- (map read <$> words <$> getLine) :: IO [Int]
    print . solve $ input
    