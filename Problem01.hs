module Main where
import Control.Applicative
import Data.List
import Test.QuickCheck

-- The Smallest Free Number.
-- Given a list of natural numbers without duplicates, find
-- the smallest natural number that is not in the list.

-- Specification from the book:
spec :: [Int] -> Int
spec = head . ([0..] \\)

-- The actual solution.
solve :: [Int] -> Int
solve xs = go 0 (length xs) xs
  where
    go lower higher [] = lower
    go lower higher xs | length left < middle - lower = go lower middle left
                       | otherwise = go middle higher right
      where
        (left, right) = partition (< middle) xs
        middle = (lower + higher + 1) `div` 2

-- Tests
newtype NatSetList = NatSetList [Int]
    deriving (Eq, Ord, Show)

deleteAny :: Eq a => [a] -> Gen (a, [a])
deleteAny xs = do
    x <- elements xs
    return (x, delete x xs)

permute :: Eq a => [a] -> Gen [a]
permute [] = return []
permute xs = do
    (y, ys) <- deleteAny xs
    ys' <- permute ys
    return (y : ys')

instance Arbitrary NatSetList where
    arbitrary = sized $ \s -> do
        n <- choose (1, max 1 s)
        (_, xs) <- deleteAny [0 .. n]
        xs' <- permute xs
        return $ NatSetList xs'

prop_spec = verboseCheck (\(NatSetList xs) -> spec xs == solve xs)

main = do
    input <- (map read <$> words <$> getLine) :: IO [Int]
    print . solve $ input