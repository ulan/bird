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


-- Unravelling greedy algorithms.
-- An unravel of a sequence xs is a bag of nonempty subsequences of xs that
-- when shuffled together can give back xs. An unravel is called an upravel if
-- all its component sequences are weakly increasing. Since each of “acm”,
-- “an” and “copy” is increasing, they give an upravel of “accompany”, and so
-- do “aany”, “ccmp” and “o”.

spec :: Ord a => [a] -> [[a]]
spec = minimumBy (compare `on` length) . filter (all up) . unravels

unravels = foldr (concatMap . prefixes) [[]]

prefixes x [] = [[[x]]]
prefixes x (xs:xss) = ((x : xs) : xss) : (map (xs :) (prefixes x xss))

up xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

solve :: Ord a => [a] -> [[a]]
solve = foldr push []
  where
    push :: Ord a => a -> [[a]] -> [[a]]
    push x stack = small ++ (prepend x large)
      where
        (small, large) = span ((x >) . head) stack

    prepend :: a -> [[a]] -> [[a]]
    prepend x [] = [[x]]
    prepend x (xs : xss) = (x : xs) : xss

prop_solve = verboseCheckWith (stdArgs {maxSize = 12}) (\xs -> spec (xs :: [Int]) == solve xs)
