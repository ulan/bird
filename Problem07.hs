{-# LANGUAGE TupleSections #-}
module Main where
import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import Data.Function
import Test.QuickCheck
import Debug.Trace

-- Given an arbitrary list of trees together with their heights, combine them
-- into a single tree of minimum height, such that the trees appear as
-- subtrees of the final tree in the same order as they appear in the list.

data Tree = Leaf Int Int | Fork Tree Tree Int
    deriving (Eq, Ord, Show)

spec :: [Tree] -> Tree
spec [tree] = tree
spec ts = minimumBy (compare `on` height) [(combine `on` spec) x y | (x, y) <- splits ts]
  where
    splits xs = tail $ reverse $ tail $ zip (inits xs) (tails xs)

height :: Tree -> Int
height (Leaf _ h) = h
height (Fork _ _ h) = h

combine :: Tree -> Tree -> Tree
combine t1 t2 = Fork t1 t2 ((max `on` height) t1 t2 + 1)

solve_greedy :: [Tree] -> Tree
solve_greedy = foldl1 combine . foldr push [] 
  where
    push tree [] = [tree]
    push tree (top : stack)
        | height tree < height collapsed = tree : collapsed : stack'
        | otherwise = push (combine tree collapsed) stack' 
      where
        collapsed = foldl combine top small
        (small, stack') = span ((<= height tree) . height) stack

memo f bounds = f g
    where r = array bounds [ (i,  f g i) | i <- range bounds]
          g = (r !)

solve_dp :: [Tree] -> Tree
solve_dp ts = recoverTree (0, n - 1)
  where n = length ts
        recoverTree (lo, hi)
            | lo == hi = tree ! lo
            | lo <= hi = (combine `on` recoverTree) (lo, mid - 1) (mid, hi)
          where mid = snd $ optimal (lo, hi)
        h = listArray (0, n) (map height ts)
        tree = listArray (0, n) ts
        optimal = memo f ((0, 0), (n - 1, n - 1))
        f g (lo, hi) | lo == hi = (h ! lo, lo)
                     | otherwise = minimum [split lo i hi  | i <- [lo + 1..hi]]
          where
            split lo i hi =(,i) $ ((\x y -> (max `on` fst) x y + 1) `on` g) (lo, i - 1) (i, hi)

prop_spec = verboseCheckWith (stdArgs {maxSize = 7}) (\(Input tree ts) -> height tree >= height (spec ts))
prop_solve_dp = verboseCheckWith (stdArgs {maxSize = 7}) (\(Input tree ts) -> (height $ spec ts) == (height $ solve_dp ts))
prop_solve_greedy = verboseCheckWith (stdArgs {maxSize = 7}) (\(Input tree ts) -> (height $ spec ts) == (height $ solve_greedy ts))
prop_solve_greedy_dp = verboseCheck (\(Input tree ts) -> (height $ solve_dp ts) == (height $ solve_greedy ts))

-- Tests
instance Arbitrary (Tree) where
    arbitrary = sized generateTree
      where
        generateTree :: Int -> Gen Tree
        generateTree n | n <= 1 = do
            value <- arbitrary
            return $ Leaf value 1
        generateTree s = do
            h <- choose (1, s - 1)
            t1 <- generateTree h
            t2 <- generateTree h
            return $ Fork t1 t2 (max (height t1) (height t2) + 1)

data Input = Input Tree [Tree]
    deriving (Eq, Ord, Show)

instance Arbitrary Input where
    arbitrary = do
        tree <- arbitrary
        ts <- split tree
        return $ Input tree ts

split (Leaf x h) = return [Leaf x h]
split (Fork t1 t2 h) =
    oneof [
        return [Fork t1 t2 h],
        do
            ts1 <- split t1
            ts2 <- split t2
            return $ ts1 ++ ts2 ]

