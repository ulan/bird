module Main where
import Control.Applicative
import Data.Array
import Data.List.Ordered
import Test.QuickCheck
import Debug.Trace

-- Making a century.
-- List all the ways the operations + and × can be inserted into
-- the list of digits [1 .. 9] so as to make a total of 100.

data Operation = Add | Mul | Glue
  deriving (Eq, Ord, Show)

eval :: [Operation] -> Int
eval os = sum + factor * number
  where
    (sum, factor, number, next) = foldl op (0, 1, 1, 2) os
    op :: (Int, Int, Int, Int) -> Operation -> (Int, Int, Int, Int)
    op (sum, factor, number, next) Add = (sum + factor * number, 1, next, next + 1)
    op (sum, factor, number, next) Mul = (sum, factor * number, next, next + 1)
    op (sum, factor, number, next) Glue = (sum, factor, number * 10 + next, next + 1)

solutions :: [String]
solutions = map pretty . filter ((100 ==) . eval) . generateAll $ 8

generateAll :: Int -> [[Operation]]
generateAll 0 = [[]]
generateAll n = [ x : xs | x <- [Add, Mul, Glue], xs <- generateAll (n - 1) ]

pretty :: [Operation] -> String
pretty = fst . foldr op ("9", 8)
  where
    op Add (text, next) = (show next ++ "+" ++ text, next - 1)
    op Mul (text, next) = (show next ++ "*" ++ text, next - 1)
    op Glue (text, next) = (show next ++ text, next - 1)

main = do
    mapM putStrLn solutions
