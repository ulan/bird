module Main where
import Control.Applicative
import Data.Array
import Data.List.Ordered
import Test.QuickCheck
import Debug.Trace

spec :: Ord a => Int -> ([a], [a]) -> a
spec k (xs, ys) = xs `union` ys !! k

-- Original solution to the selection problem from the book (Pages 24 - 25).
solve :: Ord a => Int -> ([a], [a]) -> a
solve k (xs, ys) = smallest k (xa, ya)
  where
    xa = listArray (0, length xs - 1) xs
    ya = listArray (0, length ys - 1) ys

smallest :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest k (xa, ya) = search k (0, m + 1) (0, n + 1)
  where
    (0, m) = bounds xa
    (0, n) = bounds ya
    search k (lx, rx) (ly, ry)
        | lx == rx = ya ! k
        | ly == ry = xa ! k
        | otherwise = case (xa ! mx < ya ! my, k <= mx + my) of
                        (True, True) -> search k (lx, rx) (ly, my)
                        (True, False) -> search (k - mx - 1) (mx, rx) (ly, ry)
                        (False, True) -> search k (lx, mx) (ly, ry)
                        (False, False) -> search (k - my - 1) (lx, rx) (my, ry)
      where
        mx = (lx+rx) `div` 2
        my = (ly+ry) `div` 2

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

prop_spec = verboseCheck (\(Input k xs ys) ->
    (k > 0) ==> spec k (xs, ys) == solve k (xs, ys))

main = do
    [k] <- (map read <$> words <$> getLine) :: IO [Int]
    xs <- (map read <$> words <$> getLine) :: IO [Int]
    ys <- (map read <$> words <$> getLine) :: IO [Int]
    print $ solve k (xs, ys)
   