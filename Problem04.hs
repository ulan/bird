module Main where
import Control.Applicative
import Data.Array
import Data.List.Ordered
import Test.QuickCheck
import Debug.Trace

-- A selection problem.
-- Given two disjoint sets represented as ordered lists or
-- arrays, find the k-th smallest element in their union.

-- Specification from the book. The (xs, ys) is curried.
smallest :: Ord a => Int -> [a] -> [a] -> a
smallest k xs ys = xs `union` ys !! k

-- O(k) solution.
solve1 :: Ord a => Int -> [a] -> [a] -> a
solve1 k [] ys = solve1 k ys []
solve1 0 (x:xs) [] = x
solve1 k (x:xs) [] = solve1 (k-1) xs []
solve1 k (x:xs) (y:ys) | x > y = solve1 k (y:ys) (x:xs)
solve1 0 (x:xs) _ = x
solve1 k (x:xs) ys = solve1 (k-1) xs ys

-- Convenient wrapper for the O(log n) solution.
solve2 :: Ord a => Int -> [a] -> [a] -> a
solve2 k xs ys = search k a1 (bounds a1) a2 (bounds a2)
  where
    a1 = listArray (0, length xs - 1) xs
    a2 = listArray (0, length ys - 1) ys

-- O(log |a1| + log |a2|) solution.
-- Find the k-th smallest element in union of a1[low1..high1] and a2[low2..high2].
-- All array bounds are inclusive.
search :: Ord a => Int -> Array Int a -> (Int, Int) -> Array Int a -> (Int, Int) -> a
search k a1 (low1, high1) a2 (low2, high2)
    -- Trivial case: one of the arrays is empty.
    | low1 > high1 = a2 ! (low2 + k)
    | low2 > high2 = a1 ! (low1 + k)
    -- Drop all elements after the k-th element.
    | k + 1 < length1  = search k a1 (low1, low1 + k) a2 (low2, high2)
    | k + 1 < length2  = search k a1 (low1, high1) a2 (low2, low2 + k)
    -- Trivial case: k is the last element of the union.
    | k + 1 == length1 + length2 = max (a1 ! high1) (a2 ! high2)
    -- Not so trivial case: k is the second to last element of the union,
    -- this case ensures progress when one of the array is singleton.
    | k + 1 == length1 + length2 - 1 =
        if a1 ! high1 < a2 ! high2
            then search k a1 (low1, high1) a2 (low2, high2 - 1)
            else search k a1 (low1, high1 - 1) a2 (low2, high2)
    -- Ensure that the middle of the first argument is smaller than that of the second.
    | a1 ! mid1 > a2 ! mid2 = search k a2 (low2, high2) a1 (low1, high1)
    -- Core of the solution:
    | k < prefix1 + prefix2 = search k a1 (low1, high1) a2 (low2, mid2)
    | otherwise = search (k - prefix1) a1 (mid1 + 1, high1) a2 (low2, high2)
  where
    mid1 = (low1 + high1) `div` 2
    mid2 = (low2 + high2) `div` 2
    prefix1 = (mid1 - low1 + 1)
    prefix2 = (mid2 - low2 + 1)
    length1 = (high1 - low1 + 1)
    length2 = (high2 - low2 + 1)


-- Alternative solution, inspired by the book.
-- Convenient wrapper for the O(log n) solution.
solve2' :: Ord a => Int -> [a] -> [a] -> a
solve2' k xs ys = search' k a1 (bounds a1) a2 (bounds a2)
  where
    a1 = listArray (0, length xs - 1) xs
    a2 = listArray (0, length ys - 1) ys

-- Find the k-th smallest element in union of a1[low1..high1] and a2[low2..high2].
-- All array bounds are inclusive.
search' :: Ord a => Int -> Array Int a -> (Int, Int) -> Array Int a -> (Int, Int) -> a
search' k a1 (low1, high1) a2 (low2, high2)
    -- Trivial case: one of the arrays is empty.
    | low1 > high1 = a2 ! (low2 + k)
    | low2 > high2 = a1 ! (low1 + k)
    -- Ensure that the middle of the first argument is smaller than that of the second.
    | a1 ! mid1 > a2 ! mid2 = search' k a2 (low2, high2) a1 (low1, high1)
    -- Core of the solution:
    | k < prefix1 + prefix2 - 1 = search' k a1 (low1, high1) a2 (low2, mid2 - 1)
    | otherwise = search' (k - prefix1) a1 (mid1 + 1, high1) a2 (low2, high2)
  where
    mid1 = (low1 + high1) `div` 2
    mid2 = (low2 + high2) `div` 2
    prefix1 = (mid1 - low1 + 1)
    prefix2 = (mid2 - low2 + 1)


-- Tests
data Input = Input Int [Int] [Int]
    deriving (Eq, Ord, Show)

instance Arbitrary Input where
    arbitrary = do
        xs <- nub <$> orderedList
        ys <- nub <$> orderedList
        let common = xs `isect` ys
        let xs' = xs `minus` common
        let ys' = ys `minus` common
        k <- choose (0, length xs' + length ys' - 1)
        return $ Input k xs' ys'

prop_spec1 = verboseCheck (\(Input k xs ys) ->
    length xs + length ys > 0 ==> smallest k xs ys == solve1 k xs ys)

prop_spec2 = verboseCheck (\(Input k xs ys) ->
    length xs + length ys > 0 ==> smallest k xs ys == solve2 k xs ys)

prop_spec2' = verboseCheck (\(Input k xs ys) ->
    length xs + length ys > 0 ==> smallest k xs ys == solve2' k xs ys)

main = do
    [k] <- (map read <$> words <$> getLine) :: IO [Int]
    xs <- (map read <$> words <$> getLine) :: IO [Int]
    ys <- (map read <$> words <$> getLine) :: IO [Int]
    print $ solve2 k xs ys
